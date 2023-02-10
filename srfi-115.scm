(module srfi-115
  ()

(import scheme
        (chicken module))

(import (rename (chicken irregex)
          (sre->irregex              regexp)
          (irregex?                  regexp?)
          (irregex-match             regexp-matches)
          (irregex-match?            regexp-matches?)
          (irregex-search            regexp-search)
          (irregex-match-data?       regexp-match?)
          (irregex-fold              regexp-fold)
          (irregex-extract           regexp-extract)
          (irregex-split             regexp-split)
          (irregex-replace           regexp-replace)
          (irregex-replace/all       regexp-replace-all)
          (irregex-num-submatches    regexp-match-count)
          (irregex-match-substring   regexp-match-submatch)
          (irregex-match-start-index regexp-match-submatch-start)
          (irregex-match-end-index   regexp-match-submatch-end)
          ))

(export regexp regexp? regexp-matches regexp-match? regexp-search
        regexp-fold regexp-extract regexp-split regexp-replace
        regexp-replace-all regexp-match? regexp-match-count
        regexp-match-submatch regexp-match-submatch-start
        regexp-match-submatch-end
        )

)
