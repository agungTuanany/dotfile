;;; tuanany-ui-dashboard.el -- Dashboard -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Agung Tuanany

;; Author: Agung Tuanany <agung.tuanany@gmail.com>
;; URL: http://github.com/agungTuanany/dotfile
;; Package-Requires: ((emacs "^25.1"))
;; Created: 2023
;; Version: 0.1.0
;; Keywords: dashboard

;;;; Package-Requires:

;;;; License:
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 or the License, or any later version.

;; This program is distributed in the hope it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITINES FOR
;; A PARTICULAR PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;;; Commentary:

;;;; Code:

(use-package dashboard
  :config
  (setq dashboard-startup-banner 'ascii)
  (setq dashboard-banner-logo-title "Hello Agung Jangan Malas, ingat Nuha, Aly dan Istrimu")
  (setq dashboard-footer-messages '("Take small steps and don't get discouraged. Good luck"
                                    "an idiot admires complexity, a genius admires simplicity"
                                    "Before the endgames, the Gods have placed the middle game, -Siegbert Tarrasch"
                                    "Don't tell me what the code does, show me what the code does, -Anonymous"
                                    "The great book of nature can be read only by those who know the language in which it was written. And this language is mathematics. -The Assayer"
                                    "Education is preparation for life, and only part of that is the mastery of specific work skills - Keith Devlin"
                                    "It's the method that's important, never mind if you don't get the right answer - Tom Lehrer"
                                    "Education is a prepartion for life, and only part of that is the mastery of specific work skills - Tom Lehrer"
                                    "The things about politician is that they're very much like prostitutes, but only more expensive - Riccardo Privitera"
                                    "I am invisible man, not because I don't exists, it because you choose not to see me - Bandar bin Sultan"
                                    "Without error there can be no brilliancy - Emanuel Lasker"
                                    "You have to have the fighting spirit. You have to force moves and takes chances -Bobby Fischer"
                                    "If they could see on my face what I feel in my heart no one would ever fight me - Yasuhiro Yamashita"
                                    "Programmers want to believe that if they use the right algorithms and design patterns then the bugs will take care of themselves, that garbage! - Internet of Bugs"
                                    "It's better to go slowly in the right direction, than to go speeding off in the wrong direction - Simon Sinek"
                                    ))
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook)
  )

;;; tuanany-ui-dashboard.el ends here
