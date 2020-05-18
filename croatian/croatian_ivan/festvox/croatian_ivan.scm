(require 'croatian-unisyn)

(defvar croatian-ivan-unisyn-index "/home/toni/merlin/tools/festival/lib/voices/croatian/croatian_ivan/group/ivan.group")

(defvar croatian-ivan-int-params '((f0_mean 136) (f0_std 8)))

(set! croatian-insert-filling-vowels nil)

(croatian-proclaim-voice (ivan (gender male)) ""
  (croatian-unisyn-init 'croatian_ivan croatian-ivan-unisyn-index)
  (set! croatian-int-simple-params* croatian-ivan-int-params))

(provide 'croatian_ivan)
