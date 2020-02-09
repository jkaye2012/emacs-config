
(use-package counsel-spotify
  :config
  (setq counsel-spotify-client-secret "5d33ecb883614a49ae4ed13b9c7e4db4")
  (setq counsel-spotify-client-id "1a2cb027707f4ece895a7ed17d1f9314")

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-,"
    "S" '(nil :wk "Spotify")
    "Sn" '(counsel-spotify-next :wk "next")
    "Sp" '(counsel-spotify-previous :wk "previous")
    "Ss" '(nil :wk "Search")
    "Ssa" '(counsel-spotify-search-artist :wk "artist")
    "Sst" '(counsel-spotify-search-track :wk "track")
    "St" '(counsel-spotify-toggle-play-pause :wk "play/pause")
    ))
