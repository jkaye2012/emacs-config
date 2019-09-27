
(use-package counsel-spotify
  :config
  (setq counsel-spotify-client-secret "5d33ecb883614a49ae4ed13b9c7e4db4")
  (setq counsel-spotify-client-id "1a2cb027707f4ece895a7ed17d1f9314")

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix ","
   :non-normal-prefix "M-,"
    "s" '(nil :wk "Spotify")
    "sn" '(counsel-spotify-next :wk "next")
    "sp" '(counsel-spotify-previous :wk "previous")
    "ss" '(nil :wk "Search")
    "ssa" '(counsel-spotify-search-artist :wk "artist")
    "sst" '(counsel-spotify-search-track :wk "track")
    "st" '(counsel-spotify-toggle-play-pause :wk "play/pause")
    ))
