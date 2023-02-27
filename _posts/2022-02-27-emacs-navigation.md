---
layout: post
author: Yusef Aslam
toc: true
comments: true
tags: emacs
---

This is an article that shows you how to do basic navigation in Emacs.

# What is point?

Point (or the cursor) is the block character that you see on the
screen right now, it looks like: █

-   It is there so you can operate on parts of text specifically.
-   It blinks to let you know where it is, it stops blinking when:
    1.  You are idle for a certain number of seconds.
    2.  You change focus from Emacs to another application.

# Normal Navigation (up/down/left/right) (end-of-line/beginning-of-line)

-   To move point up and down, use `C-n` and `C-p`.
-   To move point left and right, use `C-b` and `C-f`.
-   To move point to the beginning of the line, use `C-a`.
-   To move point to the end of the line, use `C-e`.
