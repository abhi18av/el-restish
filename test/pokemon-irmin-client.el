;;; pokemon-irmin-client.el --- PokéAPI client with Irmin backup integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Abhinav Sharma

;; Author: Abhinav Sharma
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (el-restish "1.0.0") (el-irmin "0.1.0"))
;; Keywords: api, pokemon, irmin, backup, games
;; URL: https://github.com/yourusername/pokemon-irmin-client

;;; Commentary:

;; Enhanced PokéAPI client that integrates el-irmin for automatic local data
;; backup with Git versioning. This provides:
;;
;; - Automatic caching of Pokémon data locally
;; - Git-based versioning of all API responses
;; - Offline access to previously fetched data
;; - Incremental sync capabilities
;; - Fast indexed queries
;; - Collection management with history
;;
;; Quick Start:
;;   (require 'pokemon-irmin-client)
;;   (pokemon-irmin-init)
;;   (pokemon-irmin-catch "pikachu")
;;   (pokemon-irmin-list-collection)

;;; Code:

(require 'el-restish)
(require 'json)

;; Try to load el-irmin modules
(require 'el-irmin nil t)
(require 'el-irmin-foundation nil t)
(require 'el-irmin-restish nil t)

;;; Customization

(defgroup pokemon-irmin nil
  "PokéAPI client with Irmin backup."
  :group 'applications
  :prefix "pokemon-irmin-")

(defcustom pokemon-irmin-store-dir
  (expand-file-name "pokemon-data" user-emacs-directory)
  "Directory for Pokémon data storage."
  :type 'directory
  :group 'pokemon-irmin)

(defcustom pokemon-irmin-auto-sync t
  "Automatically sync data to Irmin after fetching."
  :type 'boolean
  :group 'pokemon-irmin)

(defcustom pokemon-irmin-cache-ttl 3600
  "Cache TTL for Pokémon data in seconds (1 hour default)."
  :type 'integer
  :group 'pokemon-irmin)

(defcustom pokemon-irmin-favorite-generation 1
  "Your favorite Pokémon generation (1-9)."
  :type 'integer
  :group 'pokemon-irmin)

;;; State Variables

(defvar pokemon-irmin--initialized nil
  "Whether the Pokémon Irmin client has been initialized.")

(defvar pokemon-irmin--collection nil
  "List of caught Pokémon (names).")

(defvar pokemon-irmin--stats
  '(:caught 0 :species-seen 0 :types-explored 0 :generations-browsed 0)
  "Statistics about Pokémon exploration.")

;;; Core API Constants

(defconst pokemon-irmin-base-url "https://pokeapi.co/api/v2"
  "Base URL for PokéAPI.")

(defconst pokemon-irmin-types
  '("normal" "fire" "water" "electric" "grass" "ice"
    "fighting" "poison" "ground" "flying" "psychic"
    "bug" "rock" "ghost" "dragon" "dark" "steel" "fairy")
  "All Pokémon types.")

(defconst pokemon-irmin-generations
  '((1 . "Kanto (Red/Blue/Yellow)")
    (2 . "Johto (Gold/Silver/Crystal)")
    (3 . "Hoenn (Ruby/Sapphire/Emerald)")
    (4 . "Sinnoh (Diamond/Pearl/Platinum)")
    (5 . "Unova (Black/White)")
    (6 . "Kalos (X/Y)")
    (7 . "Alola (Sun/Moon)")
    (8 . "Galar (Sword/Shield)")
    (9 . "Paldea (Scarlet/Violet)"))
  "Pokémon generations with their regions.")

;;; Initialization

;;;###autoload
(defun pokemon-irmin-init ()
  "Initialize Pokémon Irmin client with local storage."
  (interactive)
  
  (unless pokemon-irmin--initialized
    (message "Initializing Pokémon Irmin client...")
    
    ;; Check if el-irmin modules are available
    (if (and (featurep 'el-irmin-foundation)
             (featurep 'el-irmin-restish))
        (pokemon-irmin--init-with-irmin)
      (pokemon-irmin--init-without-irmin))
    
    ;; Load collection from storage
    (pokemon-irmin--load-collection)
    
    (setq pokemon-irmin--initialized t)
    (message "Pokémon Irmin client initialized! Use M-x pokemon-irmin-catch to start.")))

(defun pokemon-irmin--init-with-irmin ()
  "Initialize with full Irmin integration."
  (message "Initializing with Irmin backup support...")
  
  ;; Initialize Irmin store
  (unless el-irmin--current-store
    (el-irmin-init-store pokemon-irmin-store-dir))
  
  ;; Enable el-restish backend
  (when (fboundp 'el-irmin-restish-enable)
    (el-irmin-restish-enable))
  
  ;; Register Pokémon package with foundation
  (when (fboundp 'el-irmin-foundation-register-package)
    (el-irmin-foundation-register-package
     "pokemon"
     :version "1.0.0"
     :apis (list
            ;; Pokemon endpoint
            (make-el-irmin-foundation-api
             :name "pokemon"
             :endpoint (format "%s/pokemon/" pokemon-irmin-base-url)
             :sync-strategy 'on-demand
             :cache-ttl pokemon-irmin-cache-ttl
             :rate-limit 10
             :index-fields '("id" "name" "height" "weight")
             :transform-fn #'pokemon-irmin--transform-pokemon-data)
            
            ;; Species endpoint
            (make-el-irmin-foundation-api
             :name "species"
             :endpoint (format "%s/pokemon-species/" pokemon-irmin-base-url)
             :sync-strategy 'on-demand
             :cache-ttl pokemon-irmin-cache-ttl
             :rate-limit 10
             :index-fields '("id" "name" "generation"))
            
            ;; Types endpoint
            (make-el-irmin-foundation-api
             :name "types"
             :endpoint (format "%s/type/" pokemon-irmin-base-url)
             :sync-strategy 'on-demand
             :cache-ttl (* 24 3600)  ; Types change rarely
             :rate-limit 5)
            
            ;; Generations endpoint
            (make-el-irmin-foundation-api
             :name "generations"
             :endpoint (format "%s/generation/" pokemon-irmin-base-url)
             :sync-strategy 'on-demand
             :cache-ttl (* 24 3600)  ; Generations never change
             :rate-limit 5))))
  
  (message "Pokémon data will be backed up with Git versioning!"))

(defun pokemon-irmin--init-without-irmin ()
  "Initialize without Irmin (fallback to el-restish only)."
  (message "Initializing without Irmin (el-restish only)...")
  (message "For full features, install el-irmin packages."))

(defun pokemon-irmin--transform-pokemon-data (data)
  "Transform raw Pokémon DATA for storage."
  (let ((parsed (if (stringp data) 
                    (json-parse-string data :object-type 'alist)
                  data)))
    ;; Extract only essential fields to save space
    (list (cons 'id (alist-get 'id parsed))
          (cons 'name (alist-get 'name parsed))
          (cons 'height (alist-get 'height parsed))
          (cons 'weight (alist-get 'weight parsed))
          (cons 'types (alist-get 'types parsed))
          (cons 'abilities (alist-get 'abilities parsed))
          (cons 'stats (alist-get 'stats parsed))
          (cons 'sprites (alist-get 'sprites parsed)))))

;;; Core API Functions

(defun pokemon-irmin--fetch (endpoint &optional use-cache)
  "Fetch data from ENDPOINT, optionally USE-CACHE from Irmin."
  (let ((url (if (string-prefix-p "http" endpoint)
                 endpoint
               (concat pokemon-irmin-base-url endpoint))))
    
    ;; Try to get from cache first if Irmin is available
    (when (and use-cache (featurep 'el-irmin-foundation))
      (let ((cached (pokemon-irmin--get-from-cache endpoint)))
        (when cached
          (message "Retrieved from local cache: %s" endpoint)
          (cl-return-from pokemon-irmin--fetch cached))))
    
    ;; Fetch from API
    (condition-case err
        (let* ((response (shell-command-to-string
                          (format "curl -s '%s'" url)))
               (data (json-parse-string response :object-type 'alist)))
          
          ;; Store in Irmin if available
          (when (and pokemon-irmin-auto-sync
                     (featurep 'el-irmin-foundation))
            (pokemon-irmin--store-in-cache endpoint data))
          
          data)
      (error
       (message "Failed to fetch from %s: %s" url (error-message-string err))
       nil))))

(defun pokemon-irmin--get-from-cache (endpoint)
  "Get data from cache for ENDPOINT."
  (when (fboundp 'el-irmin-kv-get)
    (let ((cache-key (pokemon-irmin--endpoint-to-key endpoint)))
      (el-irmin-kv-get cache-key :content-type 'json))))

(defun pokemon-irmin--store-in-cache (endpoint data)
  "Store DATA in cache for ENDPOINT."
  (when (fboundp 'el-irmin-kv-set)
    (let ((cache-key (pokemon-irmin--endpoint-to-key endpoint)))
      (el-irmin-kv-set cache-key data
                       :content-type 'json
                       :message (format "Cache Pokémon data: %s" endpoint)))))

(defun pokemon-irmin--endpoint-to-key (endpoint)
  "Convert ENDPOINT to cache key."
  (replace-regexp-in-string "/" "-" 
                            (replace-regexp-in-string "^/+" "" endpoint)))

;;; Main Interactive Functions

;;;###autoload
(defun pokemon-irmin-catch (pokemon-name-or-id)
  "Catch (fetch and store) a Pokémon by POKEMON-NAME-OR-ID."
  (interactive "sPokémon name or ID: ")
  
  (unless pokemon-irmin--initialized
    (pokemon-irmin-init))
  
  (message "Catching %s..." pokemon-name-or-id)
  
  (let* ((endpoint (format "/pokemon/%s/" pokemon-name-or-id))
         (data (pokemon-irmin--fetch endpoint t)))
    
    (if data
        (progn
          (let ((name (alist-get 'name data))
                (id (alist-get 'id data))
                (types (mapcar (lambda (type-entry)
                                (alist-get 'name (alist-get 'type type-entry)))
                              (alist-get 'types data)))
                (height (alist-get 'height data))
                (weight (alist-get 'weight data)))
            
            ;; Add to collection
            (pokemon-irmin--add-to-collection name)
            
            ;; Update stats
            (cl-incf (plist-get pokemon-irmin--stats :caught))
            
            ;; Display in nice buffer
            (pokemon-irmin--display-pokemon data)
            
            (message "Caught %s (#%d)! Type: %s | Height: %d | Weight: %d"
                     name id (string-join types ", ") height weight)))
      (message "Failed to catch %s" pokemon-name-or-id))))

;;;###autoload
(defun pokemon-irmin-catch-random ()
  "Catch a random Pokémon (1-1010)."
  (interactive)
  (let ((random-id (+ 1 (random 1010))))
    (message "Catching random Pokémon #%d..." random-id)
    (pokemon-irmin-catch random-id)))

;;;###autoload
(defun pokemon-irmin-list-collection ()
  "List all caught Pokémon in your collection."
  (interactive)
  
  (unless pokemon-irmin--initialized
    (pokemon-irmin-init))
  
  (if (null pokemon-irmin--collection)
      (message "No Pokémon caught yet! Use M-x pokemon-irmin-catch to start.")
    (with-output-to-temp-buffer "*Pokémon Collection*"
      (princ (format "🎮 Your Pokémon Collection (%d caught)\n" 
                     (length pokemon-irmin--collection)))
      (princ "=====================================\n\n")
      (dolist (pokemon (reverse pokemon-irmin--collection))
        (princ (format "• %s\n" pokemon)))
      (princ (format "\nTotal Stats:\n"))
      (princ (format "- Caught: %d\n" (plist-get pokemon-irmin--stats :caught)))
      (princ (format "- Species seen: %d\n" (plist-get pokemon-irmin--stats :species-seen)))
      (princ (format "- Types explored: %d\n" (plist-get pokemon-irmin--stats :types-explored))))))

;;;###autoload
(defun pokemon-irmin-view-type (type-name)
  "View all Pokémon of TYPE-NAME."
  (interactive
   (list (completing-read "Pokémon type: " pokemon-irmin-types nil t)))
  
  (unless pokemon-irmin--initialized
    (pokemon-irmin-init))
  
  (message "Exploring %s-type Pokémon..." type-name)
  
  (let* ((endpoint (format "/type/%s/" type-name))
         (data (pokemon-irmin--fetch endpoint t)))
    
    (if data
        (progn
          (cl-incf (plist-get pokemon-irmin--stats :types-explored))
          (pokemon-irmin--display-type-info data))
      (message "Failed to fetch %s type info" type-name))))

;;;###autoload
(defun pokemon-irmin-view-generation (generation-num)
  "View Pokémon from GENERATION-NUM (1-9)."
  (interactive
   (list (read-number "Generation (1-9): " pokemon-irmin-favorite-generation)))
  
  (unless (and (>= generation-num 1) (<= generation-num 9))
    (user-error "Generation must be between 1 and 9"))
  
  (unless pokemon-irmin--initialized
    (pokemon-irmin-init))
  
  (message "Exploring Generation %d..." generation-num)
  
  (let* ((endpoint (format "/generation/%d/" generation-num))
         (data (pokemon-irmin--fetch endpoint t)))
    
    (if data
        (progn
          (cl-incf (plist-get pokemon-irmin--stats :generations-browsed))
          (pokemon-irmin--display-generation-info data))
      (message "Failed to fetch generation %d info" generation-num))))

;;;###autoload
(defun pokemon-irmin-view-species (species-name)
  "View detailed species information for SPECIES-NAME."
  (interactive "sSpecies name: ")
  
  (unless pokemon-irmin--initialized
    (pokemon-irmin-init))
  
  (message "Fetching species info for %s..." species-name)
  
  (let* ((endpoint (format "/pokemon-species/%s/" species-name))
         (data (pokemon-irmin--fetch endpoint t)))
    
    (if data
        (progn
          (cl-incf (plist-get pokemon-irmin--stats :species-seen))
          (pokemon-irmin--display-species-info data))
      (message "Failed to fetch species info for %s" species-name))))

;;;###autoload
(defun pokemon-irmin-catch-starters (generation)
  "Catch all starter Pokémon from GENERATION."
  (interactive
   (list (read-number "Generation (1-9): " pokemon-irmin-favorite-generation)))
  
  (let ((starters (pcase generation
                    (1 '("bulbasaur" "charmander" "squirtle"))
                    (2 '("chikorita" "cyndaquil" "totodile"))
                    (3 '("treecko" "torchic" "mudkip"))
                    (4 '("turtwig" "chimchar" "piplup"))
                    (5 '("snivy" "tepig" "oshawott"))
                    (6 '("chespin" "fennekin" "froakie"))
                    (7 '("rowlet" "litten" "popplio"))
                    (8 '("grookey" "scorbunny" "sobble"))
                    (9 '("sprigatito" "fuecoco" "quaxly"))
                    (_ (user-error "Invalid generation")))))
    
    (message "Catching Generation %d starters..." generation)
    (dolist (starter starters)
      (pokemon-irmin-catch starter)
      (sit-for 1))  ; Brief pause between requests
    (message "Caught all Generation %d starters!" generation)))

;;; Display Functions

(defun pokemon-irmin--display-pokemon (data)
  "Display Pokémon DATA in a formatted buffer."
  (let ((name (alist-get 'name data))
        (id (alist-get 'id data))
        (height (alist-get 'height data))
        (weight (alist-get 'weight data))
        (types (alist-get 'types data))
        (abilities (alist-get 'abilities data))
        (stats (alist-get 'stats data)))
    
    (with-output-to-temp-buffer (format "*Pokémon: %s*" (capitalize name))
      (princ (format "🎮 %s (#%d)\n" (upcase name) id))
      (princ "==============================\n\n")
      
      (princ (format "Height: %d decimetres (%.1f m)\n" height (/ height 10.0)))
      (princ (format "Weight: %d hectograms (%.1f kg)\n\n" weight (/ weight 10.0)))
      
      (princ "Types:\n")
      (dolist (type-entry types)
        (let ((type-name (alist-get 'name (alist-get 'type type-entry))))
          (princ (format "  • %s\n" type-name))))
      
      (princ "\nAbilities:\n")
      (dolist (ability-entry abilities)
        (let ((ability-name (alist-get 'name (alist-get 'ability ability-entry)))
              (is-hidden (eq (alist-get 'is_hidden ability-entry) t)))
          (princ (format "  • %s%s\n" ability-name 
                        (if is-hidden " (Hidden)" "")))))
      
      (princ "\nBase Stats:\n")
      (dolist (stat-entry stats)
        (let ((stat-name (alist-get 'name (alist-get 'stat stat-entry)))
              (base-value (alist-get 'base_stat stat-entry)))
          (princ (format "  %-20s %3d  %s\n" 
                        stat-name base-value
                        (make-string (/ base-value 5) ?█))))))))

(defun pokemon-irmin--display-type-info (data)
  "Display type information from DATA."
  (let ((name (alist-get 'name data))
        (pokemon-list (alist-get 'pokemon data))
        (damage-relations (alist-get 'damage_relations data)))
    
    (with-output-to-temp-buffer (format "*Type: %s*" (capitalize name))
      (princ (format "🎮 %s-Type Pokémon\n" (upcase name)))
      (princ "==============================\n\n")
      
      (princ (format "Total Pokémon: %d\n\n" (length pokemon-list)))
      
      (when damage-relations
        (princ "Damage Relations:\n")
        (princ (format "  Double damage to: %s\n"
                      (mapconcat (lambda (t) (alist-get 'name t))
                                (alist-get 'double_damage_to damage-relations)
                                ", ")))
        (princ (format "  Double damage from: %s\n"
                      (mapconcat (lambda (t) (alist-get 'name t))
                                (alist-get 'double_damage_from damage-relations)
                                ", ")))
        (princ "\n"))
      
      (princ "Pokémon of this type (first 20):\n")
      (cl-loop for poke-entry in pokemon-list
               for i from 1 to 20
               do (let ((poke-name (alist-get 'name (alist-get 'pokemon poke-entry))))
                    (princ (format "  %2d. %s\n" i poke-name)))))))

(defun pokemon-irmin--display-generation-info (data)
  "Display generation information from DATA."
  (let ((name (alist-get 'name data))
        (id (alist-get 'id data))
        (main-region (alist-get 'name (alist-get 'main_region data)))
        (pokemon-species (alist-get 'pokemon_species data)))
    
    (with-output-to-temp-buffer (format "*Generation %d*" id)
      (princ (format "🎮 Generation %d: %s\n" id (capitalize name)))
      (princ "==============================\n\n")
      (princ (format "Main Region: %s\n" (capitalize main-region)))
      (princ (format "Total Species: %d\n\n" (length pokemon-species)))
      
      (princ "Pokémon Species:\n")
      (cl-loop for species-entry in pokemon-species
               for i from 1
               do (let ((species-name (alist-get 'name species-entry)))
                    (princ (format "  %3d. %s\n" i species-name)))))))

(defun pokemon-irmin--display-species-info (data)
  "Display species information from DATA."
  (let ((name (alist-get 'name data))
        (id (alist-get 'id data))
        (generation (alist-get 'name (alist-get 'generation data)))
        (flavor-texts (alist-get 'flavor_text_entries data))
        (is-legendary (eq (alist-get 'is_legendary data) t))
        (is-mythical (eq (alist-get 'is_mythical data) t)))
    
    (with-output-to-temp-buffer (format "*Species: %s*" (capitalize name))
      (princ (format "🎮 %s (#%d)\n" (upcase name) id))
      (princ "==============================\n\n")
      (princ (format "Generation: %s\n" (capitalize generation)))
      (when is-legendary (princ "⭐ LEGENDARY\n"))
      (when is-mythical (princ "✨ MYTHICAL\n"))
      (princ "\nDescription:\n")
      (when flavor-texts
        (let ((english-text (cl-find-if 
                             (lambda (entry)
                               (string= "en" (alist-get 'name (alist-get 'language entry))))
                             flavor-texts)))
          (when english-text
            (princ (format "\n%s\n" (alist-get 'flavor_text english-text)))))))))

;;; Collection Management

(defun pokemon-irmin--add-to-collection (pokemon-name)
  "Add POKEMON-NAME to collection if not already present."
  (unless (member pokemon-name pokemon-irmin--collection)
    (push pokemon-name pokemon-irmin--collection)
    (pokemon-irmin--save-collection)))

(defun pokemon-irmin--save-collection ()
  "Save collection to storage."
  (when (fboundp 'el-irmin-kv-set)
    (el-irmin-kv-set "pokemon/collection" pokemon-irmin--collection
                     :content-type 'json
                     :message "Update Pokémon collection")
    (el-irmin-kv-set "pokemon/stats" pokemon-irmin--stats
                     :content-type 'json
                     :message "Update Pokémon stats")))

(defun pokemon-irmin--load-collection ()
  "Load collection from storage."
  (when (fboundp 'el-irmin-kv-get)
    (let ((collection (el-irmin-kv-get "pokemon/collection" :content-type 'json))
          (stats (el-irmin-kv-get "pokemon/stats" :content-type 'json)))
      (when collection
        (setq pokemon-irmin--collection collection))
      (when stats
        (setq pokemon-irmin--stats stats)))))

;;; Query Functions (requires Irmin)

;;;###autoload
(defun pokemon-irmin-search-by-name (name-pattern)
  "Search caught Pokémon by NAME-PATTERN."
  (interactive "sSearch name: ")
  
  (unless pokemon-irmin--initialized
    (pokemon-irmin-init))
  
  (let ((matches (cl-remove-if-not 
                  (lambda (name) (string-match-p name-pattern name))
                  pokemon-irmin--collection)))
    (if matches
        (message "Found: %s" (string-join matches ", "))
      (message "No matches found for '%s'" name-pattern))))

;;;###autoload
(defun pokemon-irmin-stats ()
  "Display Pokémon collection statistics."
  (interactive)
  
  (unless pokemon-irmin--initialized
    (pokemon-irmin-init))
  
  (message "📊 Pokémon Stats: Caught: %d | Species: %d | Types: %d | Generations: %d"
           (plist-get pokemon-irmin--stats :caught)
           (plist-get pokemon-irmin--stats :species-seen)
           (plist-get pokemon-irmin--stats :types-explored)
           (plist-get pokemon-irmin--stats :generations-browsed)))

;;; Interactive Demo

;;;###autoload
(defun pokemon-irmin-demo ()
  "Run an interactive demo of Pokémon Irmin client."
  (interactive)
  
  (message "🎮 Starting Pokémon Irmin Demo...")
  
  (unless pokemon-irmin--initialized
    (pokemon-irmin-init))
  
  (sit-for 1)
  
  ;; Demo 1: Catch famous Pokémon
  (message "1. Catching Pikachu...")
  (pokemon-irmin-catch "pikachu")
  (sit-for 2)
  
  ;; Demo 2: Catch legendary
  (message "2. Catching Mewtwo (Legendary)...")
  (pokemon-irmin-catch "mewtwo")
  (sit-for 2)
  
  ;; Demo 3: Explore a type
  (message "3. Exploring Dragon-type...")
  (pokemon-irmin-view-type "dragon")
  (sit-for 2)
  
  ;; Demo 4: Random catch
  (message "4. Catching random Pokémon...")
  (pokemon-irmin-catch-random)
  (sit-for 2)
  
  ;; Demo 5: Show collection
  (message "5. Viewing collection...")
  (pokemon-irmin-list-collection)
  
  (message "🎮 Demo complete! Your Pokémon data is backed up with Git versioning."))

;;; Minor Mode

(defvar pokemon-irmin-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c P c") #'pokemon-irmin-catch)
    (define-key map (kbd "C-c P r") #'pokemon-irmin-catch-random)
    (define-key map (kbd "C-c P l") #'pokemon-irmin-list-collection)
    (define-key map (kbd "C-c P t") #'pokemon-irmin-view-type)
    (define-key map (kbd "C-c P g") #'pokemon-irmin-view-generation)
    (define-key map (kbd "C-c P s") #'pokemon-irmin-view-species)
    (define-key map (kbd "C-c P S") #'pokemon-irmin-stats)
    (define-key map (kbd "C-c P d") #'pokemon-irmin-demo)
    map)
  "Keymap for Pokémon Irmin mode.")

;;;###autoload
(define-minor-mode pokemon-irmin-mode
  "Minor mode for Pokémon exploration with Irmin backup."
  :lighter " 🎮⚡"
  :keymap pokemon-irmin-mode-map
  (if pokemon-irmin-mode
      (progn
        (unless pokemon-irmin--initialized
          (pokemon-irmin-init))
        (message "Pokémon Irmin mode enabled! Use C-c P <key> for commands."))
    (message "Pokémon Irmin mode disabled.")))

(provide 'pokemon-irmin-client)

;;; pokemon-irmin-client.el ends here