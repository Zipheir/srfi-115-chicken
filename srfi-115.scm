(module srfi-115
  ()

(import (chicken module))

(import (rename (chicken irregex)
          (sre->irregex              regexp)
          (irregex?                  regexp?)
          (irregex-match             regexp-matches)
          (irregex-match?            regexp-match?)
          (irregex-search            regexp-search)
          (irregex-fold              regexp-fold)
          (irregex-extract           regexp-extract)
          (irregex-split             regexp-split)
          (irregex-replace           regexp-replace)
          (irregex-replace/all       regexp-replace-all)
          (irregex-match?            regexp-match?)
          (irregex-num-submatches    regexp-match-count)
          (irregex-match-substring   regexp-match-submatch)
          (irregex-match-start-index regexp-match-submatch-start)
          (irregex-match-end-index   regexp-match-submatch-end)
          )

)
