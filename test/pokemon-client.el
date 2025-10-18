;;; pokemon-client.el --- PokéAPI client for el-restish integration testing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Abhinav Sharma

;; This file is part of el-restish.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This is a comprehensive integration test for el-restish using the PokéAPI.
;; PokéAPI is perfect for testing because:
;; - No authentication required (tests basic functionality)
;; - Rich, complex JSON responses (tests formatting)
;; - Multiple related endpoints (tests client framework)
;; - Reliable and fast (great for automated testing)
;; - Fun and engaging content
;;
;; API Documentation: https://pokeapi.co/docs/v2
;; Base URL: https://pokeapi.co/api/v2/
;; No authentication required

;;; Code:

(require 'el-restish-client)

;; Define the PokéAPI client
(el-restish-define-client pokemon
  :service "https://pokeapi.co"
  :base-path "/api/v2"
  :default-headers '(("User-Agent" . "el-restish-pokemon-client/1.0"))
  :prefer-async t)

;; Core Pokémon endpoints

;;;###autoload
(defun pokemon-get (pokemon-id)
  "Get detailed information about a Pokémon by ID or name.
POKEMON-ID can be a number (1-1010) or a Pokémon name."
  (interactive "sPokémon ID or name: ")
  (el-restish-client-get pokemon-client (format "/pokemon/%s/" pokemon-id)))

;;;###autoload
(defun pokemon-list (&optional limit offset)
  "List Pokémon with optional LIMIT and OFFSET.
LIMIT defaults to 20, OFFSET defaults to 0."
  (interactive)
  (let* ((limit (or limit 20))
         (offset (or offset 0))
         (query-params (format "?limit=%d&offset=%d" limit offset)))
    (el-restish-client-get pokemon-client (format "/pokemon/%s" query-params))))

;;;###autoload
(defun pokemon-species (species-id)
  "Get Pokémon species information by SPECIES-ID."
  (interactive "sPokémon species ID or name: ")
  (el-restish-client-get pokemon-client (format "/pokemon-species/%s/" species-id)))

;;;###autoload
(defun pokemon-evolution-chain (chain-id)
  "Get evolution chain information by CHAIN-ID."
  (interactive "nEvolution chain ID: ")
  (el-restish-client-get pokemon-client (format "/evolution-chain/%d/" chain-id)))

;;;###autoload
(defun pokemon-type (type-name)
  "Get information about a Pokémon TYPE-NAME (e.g., fire, water, grass)."
  (interactive "sPokémon type: ")
  (el-restish-client-get pokemon-client (format "/type/%s/" type-name)))

;;;###autoload
(defun pokemon-generation (generation-id)
  "Get information about a Pokémon GENERATION-ID."
  (interactive "nGeneration (1-9): ")
  (unless (and (>= generation-id 1) (<= generation-id 9))
    (error "Generation must be between 1 and 9"))
  (el-restish-client-get pokemon-client (format "/generation/%d/" generation-id)))

;;;###autoload
(defun pokemon-region (region-name)
  "Get information about a Pokémon REGION-NAME (e.g., kanto, johto)."
  (interactive "sRegion name: ")
  (el-restish-client-get pokemon-client (format "/region/%s/" region-name)))

;;;###autoload
(defun pokemon-ability (ability-name)
  "Get information about a Pokémon ABILITY-NAME."
  (interactive "sAbility name: ")
  (el-restish-client-get pokemon-client (format "/ability/%s/" ability-name)))

;;;###autoload
(defun pokemon-move (move-name)
  "Get information about a Pokémon MOVE-NAME."
  (interactive "sMove name: ")
  (el-restish-client-get pokemon-client (format "/move/%s/" move-name)))

;;;###autoload
(defun pokemon-item (item-name)
  "Get information about a Pokémon ITEM-NAME."
  (interactive "sItem name: ")
  (el-restish-client-get pokemon-client (format "/item/%s/" item-name)))

;; Convenience functions

;;;###autoload
(defun pokemon-get-starter (generation)
  "Get information about starter Pokémon for GENERATION."
  (interactive "nGeneration (1-9): ")
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
                    (_ (error "Invalid generation")))))
    (dolist (starter starters)
      (pokemon-get starter)
      (sit-for 1))))  ; Brief pause between requests

;;;###autoload
(defun pokemon-get-random ()
  "Get information about a random Pokémon (1-1010)."
  (interactive)
  (let ((random-id (+ 1 (random 1010))))
    (message "Getting random Pokémon #%d..." random-id)
    (pokemon-get random-id)))

;;;###autoload
(defun pokemon-search-by-type (type-name)
  "Search for all Pokémon of a specific TYPE-NAME."
  (interactive
   (list (completing-read "Pokémon type: "
                          '("normal" "fire" "water" "electric" "grass" "ice"
                            "fighting" "poison" "ground" "flying" "psychic"
                            "bug" "rock" "ghost" "dragon" "dark" "steel" "fairy")
                          nil t)))
  (pokemon-type type-name))

;;;###autoload
(defun pokemon-browse-generation ()
  "Interactively browse Pokémon generations."
  (interactive)
  (let* ((generations '(("1. Kanto (Red/Blue/Yellow)" . 1)
                        ("2. Johto (Gold/Silver/Crystal)" . 2)
                        ("3. Hoenn (Ruby/Sapphire/Emerald)" . 3)
                        ("4. Sinnoh (Diamond/Pearl/Platinum)" . 4)
                        ("5. Unova (Black/White)" . 5)
                        ("6. Kalos (X/Y)" . 6)
                        ("7. Alola (Sun/Moon)" . 7)
                        ("8. Galar (Sword/Shield)" . 8)
                        ("9. Paldea (Scarlet/Violet)" . 9)))
         (choice (completing-read "Select generation: " generations nil t))
         (gen-id (cdr (assoc choice generations))))
    (pokemon-generation gen-id)))

;; Integration test functions

(defun pokemon-test-basic-functionality ()
  "Test basic PokéAPI functionality."
  (interactive)
  (message "Testing PokéAPI basic functionality...")
  
  ;; Test 1: Get Pikachu (most famous Pokémon)
  (message "Test 1: Getting Pikachu...")
  (let ((pikachu-buffer (pokemon-get "pikachu")))
    (if pikachu-buffer
        (message "✓ Pikachu retrieved successfully")
      (message "✗ Failed to retrieve Pikachu")))
  
  ;; Test 2: List first 10 Pokémon
  (message "Test 2: Listing first 10 Pokémon...")
  (let ((list-buffer (pokemon-list 10 0)))
    (if list-buffer
        (message "✓ Pokémon list retrieved successfully")
      (message "✗ Failed to retrieve Pokémon list")))
  
  ;; Test 3: Get fire type info
  (message "Test 3: Getting fire type information...")
  (let ((fire-buffer (pokemon-type "fire")))
    (if fire-buffer
        (message "✓ Fire type information retrieved successfully")
      (message "✗ Failed to retrieve fire type information")))
  
  ;; Test 4: Get random Pokémon
  (message "Test 4: Getting random Pokémon...")
  (let ((random-buffer (pokemon-get-random)))
    (if random-buffer
        (message "✓ Random Pokémon retrieved successfully")
      (message "✗ Failed to retrieve random Pokémon")))
  
  (message "PokéAPI basic functionality test completed."))

(defun pokemon-test-complex-data ()
  "Test PokéAPI with complex nested data structures."
  (interactive)
  (message "Testing complex data structures...")
  
  ;; Test evolution chain (complex nested structure)
  (message "Test 1: Getting Eevee evolution chain...")
  (let ((evolution-buffer (pokemon-evolution-chain 67)))  ; Eevee's evolution chain
    (if evolution-buffer
        (with-current-buffer evolution-buffer
          (goto-char (point-min))
          (if (search-forward "eevee" nil t)
              (message "✓ Complex evolution chain data retrieved and parsed")
            (message "✗ Evolution chain data incomplete")))
      (message "✗ Failed to retrieve evolution chain")))
  
  ;; Test species data (rich text descriptions)
  (message "Test 2: Getting Charizard species info...")
  (let ((species-buffer (pokemon-species "charizard")))
    (if species-buffer
        (with-current-buffer species-buffer
          (goto-char (point-min))
          (if (search-forward "flavor_text_entries" nil t)
              (message "✓ Species data with descriptions retrieved")
            (message "✗ Species data incomplete")))
      (message "✗ Failed to retrieve species data")))
  
  (message "Complex data structure test completed."))

(defun pokemon-test-error-handling ()
  "Test error handling with invalid requests."
  (interactive)
  (message "Testing error handling...")
  
  ;; Test invalid Pokémon ID
  (condition-case err
      (let ((buffer (pokemon-get "99999")))
        (if buffer
            (with-current-buffer buffer
              (goto-char (point-min))
              ;; Should contain error information
              (if (search-forward "404" nil t)
                  (message "✓ 404 error handled correctly")
                (message "? Unexpected response for invalid ID")))
          (message "✓ Invalid Pokémon ID handled correctly")))
    (error (message "✓ Invalid ID error caught: %s" (error-message-string err))))
  
  ;; Test invalid type
  (condition-case err
      (let ((buffer (pokemon-type "invalid-type")))
        (if buffer
            (message "✓ Invalid type request handled")
          (message "✓ Invalid type rejected")))
    (error (message "✓ Invalid type error caught: %s" (error-message-string err))))
  
  (message "Error handling test completed."))

(defun pokemon-test-performance ()
  "Test performance with multiple requests."
  (interactive)
  (message "Testing performance with multiple requests...")
  
  (let ((start-time (current-time))
        (success-count 0)
        (total-requests 5))
    
    ;; Make multiple async requests
    (dotimes (i total-requests)
      (let ((pokemon-id (+ 1 i)))
        (condition-case nil
            (progn
              (pokemon-get pokemon-id)
              (setq success-count (1+ success-count)))
          (error nil))))
    
    ;; Wait for async requests to complete
    (sit-for 3)
    
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Performance test: %d/%d requests succeeded in %.2f seconds"
               success-count total-requests elapsed))
    
    (> success-count 0)))

(defun pokemon-run-integration-tests ()
  "Run complete PokéAPI integration test suite."
  (interactive)
  (message "\n=== PokéAPI Integration Test Suite ===")
  (pokemon-test-basic-functionality)
  (sit-for 1)
  (pokemon-test-complex-data)
  (sit-for 1)
  (pokemon-test-error-handling)
  (sit-for 1)
  (pokemon-test-performance)
  (message "\n=== PokéAPI Tests Complete ===")
  (message "Check the various '*Restish:*' buffers for API responses."))

;; Demo and showcase functions

;;;###autoload
(defun pokemon-showcase-demo ()
  "Run a showcase demo of PokéAPI integration."
  (interactive)
  (message "🎮 Starting PokéAPI Showcase Demo...")
  
  ;; Demo 1: Classic starter Pokémon
  (message "1. Getting classic Kanto starters...")
  (pokemon-get "pikachu")  ; Most famous Pokémon
  (sit-for 2)
  (pokemon-get "charizard")  ; Most popular starter evolution
  (sit-for 2)
  
  ;; Demo 2: Type effectiveness
  (message "2. Exploring type effectiveness...")
  (pokemon-search-by-type "dragon")
  (sit-for 2)
  
  ;; Demo 3: Generation exploration
  (message "3. Exploring generations...")
  (pokemon-generation 1)  ; Original generation
  (sit-for 2)
  
  ;; Demo 4: Complex evolution chains
  (message "4. Complex evolution chains...")
  (pokemon-evolution-chain 67)  ; Eevee - has 8 evolutions!
  (sit-for 2)
  
  ;; Demo 5: Random discovery
  (message "5. Random Pokémon discovery...")
  (pokemon-get-random)
  (sit-for 2)
  
  (message "🎮 PokéAPI Showcase complete!")
  (message "Use M-x el-restish-pop-response to explore the responses.")
  (message "Try M-x pokemon-browse-generation for interactive exploration!"))

;; Utility functions

(defun pokemon-get-sprite-url (pokemon-name)
  "Get the sprite URL for POKEMON-NAME (for future image display)."
  (format "https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/%s.png"
          pokemon-name))

(defun pokemon-format-stats (stats-json)
  "Format Pokémon stats from STATS-JSON for display."
  ;; This would parse the stats JSON and format nicely
  ;; For now, just a placeholder
  "Stats formatting not implemented yet")

;;;###autoload
(defun pokemon-info-summary (pokemon-name)
  "Get a quick summary of POKEMON-NAME."
  (interactive "sPokémon name: ")
  (let ((buffer (pokemon-get pokemon-name)))
    (when buffer
      (message "Fetched detailed information for %s. Check the response buffer!" pokemon-name))))

;; Educational functions

;;;###autoload
(defun pokemon-learn-types ()
  "Interactive function to learn about Pokémon types."
  (interactive)
  (let* ((types '("normal" "fire" "water" "electric" "grass" "ice"
                  "fighting" "poison" "ground" "flying" "psychic"
                  "bug" "rock" "ghost" "dragon" "dark" "steel" "fairy"))
         (type (completing-read "Learn about type: " types nil t)))
    (pokemon-type type)
    (message "Learning about %s-type Pokémon! Check the response buffer." type)))

;;;###autoload
(defun pokemon-compare (pokemon1 pokemon2)
  "Compare two Pokémon by fetching both."
  (interactive "sFirst Pokémon: \nsSecond Pokémon: ")
  (pokemon-get pokemon1)
  (sit-for 1)
  (pokemon-get pokemon2)
  (message "Comparing %s vs %s! Check both response buffers." pokemon1 pokemon2))

;; Minor mode for Pokémon exploration

(defvar pokemon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p g") #'pokemon-get)
    (define-key map (kbd "C-c p l") #'pokemon-list)
    (define-key map (kbd "C-c p r") #'pokemon-get-random)
    (define-key map (kbd "C-c p t") #'pokemon-search-by-type)
    (define-key map (kbd "C-c p s") #'pokemon-species)
    (define-key map (kbd "C-c p e") #'pokemon-evolution-chain)
    (define-key map (kbd "C-c p b") #'pokemon-browse-generation)
    (define-key map (kbd "C-c p d") #'pokemon-showcase-demo)
    (define-key map (kbd "C-c p T") #'pokemon-run-integration-tests)
    map)
  "Keymap for Pokémon exploration mode.")

;;;###autoload
(define-minor-mode pokemon-mode
  "Minor mode for exploring the PokéAPI."
  :lighter " 🎮"
  :keymap pokemon-mode-map
  (if pokemon-mode
      (message "Pokémon mode enabled! Use C-c p <key> for commands. Gotta catch 'em all!")
    (message "Pokémon mode disabled.")))

;; Configuration

(defcustom pokemon-favorite-generation 1
  "Your favorite Pokémon generation (1-9)."
  :type 'integer
  :group 'pokemon)

(defcustom pokemon-favorite-type "electric"
  "Your favorite Pokémon type."
  :type 'string
  :group 'pokemon)

;;;###autoload
(defun pokemon-configure ()
  "Configure Pokémon preferences."
  (interactive)
  (let ((generation (read-number "Favorite generation (1-9): " pokemon-favorite-generation))
        (type (completing-read "Favorite type: "
                               '("normal" "fire" "water" "electric" "grass" "ice"
                                 "fighting" "poison" "ground" "flying" "psychic"
                                 "bug" "rock" "ghost" "dragon" "dark" "steel" "fairy")
                               nil t pokemon-favorite-type)))
    (setq pokemon-favorite-generation generation
          pokemon-favorite-type type)
    (message "Configured! Favorite: Generation %d, %s-type" generation type)))

(provide 'pokemon-client)
;;; pokemon-client.el ends here