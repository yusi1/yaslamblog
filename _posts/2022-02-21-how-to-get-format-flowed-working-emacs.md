---
layout: post
author: Yusef Aslam
title: How to get format=flowed working in Mu4e inside Emacs
tags: emacs-lisp, format=flowed, mail
---

The best way to get [f=f](https://joeclark.org/ffaq.html) (AKA [format=flowed](https://joeclark.org/ffaq.html)) working inside Mu4e on Emacs, is to put this code into a file, and then into `load-path` and require it.

Also, in the config documentation for Mu4e this is mentioned:
> For this to happen properly, you should write each paragraph of your message of as a long line (i.e. without carriage return). If you introduce unwanted newlines in your paragraph, use M-q to reformat it as a single line.

The functions below reformat the message before sending it to comply with the above, even if you didn't format it properly.

```
(setq mu4e-compose-format-flowed t)
(add-hook mu4e-compose-mode-hook (lambda () (auto-fill-mode -1)))
(setq fill-flowed-encode-column 998)

(defun mu4e-fill-buffer-for-flowed ()
  "Fill all lines in buffer to match format=flowed email spec.

Specifically, fills all *paragraph* lines to the number of columns
specified in fill-code-encode-column and ends each paragraph line
with a space followed by a newline, following the specification.

Non-paragraph lines are not filled, allowing for manual formatting.
A line is a non-paragraph line if it, plus the following word on the
next line, are short enough that they would not have been filled by
(fill-paragraph) based on the current value of fill-column.  The effect
of this feature is that you can compose messages with auto-fill-mode on
(to have a comfortable line length for writing) and any lines that were
long enough to have been altered by auto-fill-mode will be refilled
(potentially to a different length) and padded with a final space to
match the fill=flowed RFC."
  (interactive)
  (end-of-buffer)
  (message "start fn")
  (mu4e-compose-goto-top)
  (narrow-to-region (point-max) (point))
  (message "start loop")
  (while (not (eobp))
    (message "%s" (thing-at-point 'line t))
    (when (> (save-excursion (forward-line 1)
                             (forward-word)
                             (point))
             (+ (point-at-bol) fill-column))
      (beginning-of-line)
      (let ((fill-column fill-flowed-encode-column))
        (fill-paragraph))
      (end-of-line)
      (insert " "))
    (end-of-line 2))
  (message "end of loop")
  (widen))

(defun mu4e-fill-send-and-exit ()
  "Reflow the message text with (mu4e-fill-buffer-for-flowed) and send it."
  (interactive)
  (mu4e-fill-buffer-for-flowed)
  (message-send-and-exit))

(advice-add 'message-send-and-exit :before #'mu4e-fill-buffer-for-flowed)

(provide 'mu4e-format-flowed)
```
