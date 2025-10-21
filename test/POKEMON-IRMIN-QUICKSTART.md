# Pokémon Irmin Client - Quick Start Guide

Test the Pokémon API client with Irmin backup integration interactively in Emacs!

## Setup

### 1. Load Required Paths

```elisp
;; Add el-irmin and el-restish to load path
(add-to-list 'load-path "/Users/abhinavsharma/projects/el-irmin")
(add-to-list 'load-path "/Users/abhinavsharma/projects/el-restish")

;; Load the client
(load-file "/Users/abhinavsharma/projects/el-restish/pokemon-irmin-client.el")
```

### 2. Initialize the Client

```elisp
;; Initialize Pokémon Irmin client
(pokemon-irmin-init)
```

This will:
- Create a local Irmin store at `~/.emacs.d/pokemon-data/`
- Set up Git versioning for all Pokémon data
- Register API endpoints with caching and rate limiting
- Load your previous collection if it exists

## Basic Commands to Try

### Catch Pokémon

```elisp
;; Catch Pikachu!
(pokemon-irmin-catch "pikachu")

;; Catch by ID
(pokemon-irmin-catch "25")

;; Catch a random Pokémon
(pokemon-irmin-catch-random)

;; Catch Generation 1 starters
(pokemon-irmin-catch-starters 1)
```

### View Your Collection

```elisp
;; List all caught Pokémon
(pokemon-irmin-list-collection)

;; View statistics
(pokemon-irmin-stats)

;; Search your collection
(pokemon-irmin-search-by-name "char")
```

### Explore Pokémon Data

```elisp
;; View type information
(pokemon-irmin-view-type "dragon")

;; Explore generations
(pokemon-irmin-view-generation 1)

;; View species details
(pokemon-irmin-view-species "charizard")
```

## Interactive Demo

Run the complete demo to see all features:

```elisp
(pokemon-irmin-demo)
```

This will:
1. Catch Pikachu
2. Catch Mewtwo (legendary)
3. Explore Dragon-type
4. Catch a random Pokémon
5. Show your collection

## Using the Minor Mode

Enable the minor mode for convenient keybindings:

```elisp
(pokemon-irmin-mode 1)
```

Then use these keybindings:
- `C-c P c` - Catch Pokémon
- `C-c P r` - Catch random
- `C-c P l` - List collection
- `C-c P t` - View type
- `C-c P g` - View generation
- `C-c P s` - View species
- `C-c P S` - Show stats
- `C-c P d` - Run demo

## Interactive Testing Session

Copy and paste these commands one by one and execute with `C-x C-e`:

```elisp
;; 1. Initialize
(pokemon-irmin-init)

;; 2. Catch some famous Pokémon
(pokemon-irmin-catch "pikachu")
(pokemon-irmin-catch "charizard")
(pokemon-irmin-catch "mewtwo")

;; 3. View your collection
(pokemon-irmin-list-collection)

;; 4. Explore types
(pokemon-irmin-view-type "electric")

;; 5. Catch more Pokémon
(pokemon-irmin-catch "dragonite")
(pokemon-irmin-catch "gengar")

;; 6. Check stats
(pokemon-irmin-stats)

;; 7. Search collection
(pokemon-irmin-search-by-name "drag")

;; 8. Explore a generation
(pokemon-irmin-view-generation 1)

;; 9. Random catches!
(pokemon-irmin-catch-random)
(pokemon-irmin-catch-random)
(pokemon-irmin-catch-random)

;; 10. Final collection view
(pokemon-irmin-list-collection)
```

## Verifying Git Backup

Check that data is actually being backed up with Git:

```bash
# In terminal
cd ~/.emacs.d/pokemon-data
git log --oneline
git log --stat
```

You should see commits for:
- "Cache Pokémon data"
- "Update Pokémon collection"
- "Update Pokémon stats"

## Accessing Cached Data

The client automatically caches data locally. Try:

```elisp
;; First fetch (from API)
(pokemon-irmin-catch "bulbasaur")

;; Second fetch (from cache - much faster!)
(pokemon-irmin-catch "bulbasaur")
```

Watch the messages - the second time will say "Retrieved from local cache"!

## Advanced Features

### View All Cached Data

```elisp
;; List all keys in Irmin store
(when (featurep 'el-irmin)
  (el-irmin-kv-list))
```

### Check Irmin Store Status

```elisp
;; Version info
(when (featurep 'el-irmin)
  (irmin-version))

;; Git status (if el-irmin-git is loaded)
(when (featurep 'el-irmin-git)
  (el-irmin-git-status))
```

### Manual Cache Inspection

```elisp
;; Get cached Pikachu data
(when (featurep 'el-irmin)
  (el-irmin-kv-get "pokemon-pikachu-" :content-type 'json))

;; Get your collection
(when (featurep 'el-irmin)
  (el-irmin-kv-get "pokemon/collection" :content-type 'json))
```

## Fun Challenges

Try these challenges to explore the features:

### Challenge 1: Complete a Type Collection
Catch all Dragon-type Pokémon (hint: use `pokemon-irmin-view-type` first!)

### Challenge 2: Gotta Catch Starters!
Catch all starters from Generations 1-9:
```elisp
(dotimes (gen 9)
  (pokemon-irmin-catch-starters (+ gen 1))
  (sit-for 2))
```

### Challenge 3: Lucky Random 10
Catch 10 random Pokémon:
```elisp
(dotimes (i 10)
  (pokemon-irmin-catch-random)
  (sit-for 1))
```

### Challenge 4: Evolution Chain
Catch an evolution chain:
```elisp
(pokemon-irmin-catch "charmander")
(pokemon-irmin-catch "charmeleon")
(pokemon-irmin-catch "charizard")
```

## Troubleshooting

### If el-irmin modules aren't loading:

```elisp
;; Check what's loaded
(featurep 'el-irmin)
(featurep 'el-irmin-foundation)

;; Try loading manually
(require 'el-irmin)
(require 'el-irmin-foundation)
(require 'el-irmin-restish)
```

### If the client works but without Irmin:

The client will still work using `curl` for API requests, but won't have:
- Git versioning
- Persistent caching
- Offline access

You'll see: "Initializing without Irmin (el-restish only)..."

### To use full Irmin features:

1. Make sure el-irmin files are in your load-path
2. Make sure Irmin CLI is installed: `opam install irmin-cli`
3. Reinitialize: `(pokemon-irmin-init)`

## Clean Up (if needed)

To start fresh:

```elisp
;; Reset client state
(setq pokemon-irmin--initialized nil)
(setq pokemon-irmin--collection nil)
(setq pokemon-irmin--stats '(:caught 0 :species-seen 0 :types-explored 0 :generations-browsed 0))

;; Re-initialize
(pokemon-irmin-init)
```

To remove all data:

```bash
# In terminal
rm -rf ~/.emacs.d/pokemon-data
```

## Next Steps

Once you're comfortable with the client:

1. Try building your own API client using the same pattern
2. Explore the Irmin store with `el-irmin-git-status`
3. Use the foundation APIs to create your own data-backed application
4. Check out the full integration guide in `../el-irmin/README-API-INTEGRATION.md`

## Have Fun!

Gotta catch 'em all! 🎮⚡