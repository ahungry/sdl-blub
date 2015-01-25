;;;; sdl-blub.lisp

(in-package #:sdl-blub)

;;; "sdl-blub" goes here. Hacks and glory await!

;; Tutorials/docs here: http://code.google.com/p/lispbuilder/wiki/UsingLispbuilderSDL#The_Game_Loop
;; Standalone (eventually) here: http://code.google.com/p/lispbuilder/wiki/StandAloneExecutables

(in-readtable glyphs:syntax)

(ƒ get-animation
   "down"  → #(7 6 7 8)
   "right" → #(4 3 4 5)
   "left"  → #(10 9 10 11)
   "up"    → #(1 0 1 2)
   α       → #(7))

(defparameter *frame-index* 0)
(defparameter *x* 0)
(defparameter *y* 0)
(defparameter *px* 0)
(defparameter *py* 0)
(defparameter *sprites* '())

(defun clear-and-set-global-assets ()
  (defparameter *cells* nil)
  (defparameter *img-player* nil)
  (defparameter *img-bg* nil)
  (defparameter *bg-layers* (make-array 7 :initial-element nil)) ;; 7 layer mmmm
  (defparameter *img-bg-layer-0* nil)
  (defparameter *img-bg-layer-1* nil)
  (defparameter *img-bg-layer-2* nil)
  (defparameter *img-bg-layer-3* nil)
  (defparameter *img-bg-foreground* nil)
  (defparameter *img-bg-foreground-1* nil)
  (defparameter *img-bg-frame* 0)
  (defparameter *asset-path* (format nil "~a~a" (user-homedir-pathname) "src/lisp/sdl-blub/assets"))
  (defparameter *old-sprite-scale* 3)
  (defparameter *sprite-scale* 3)
  (defparameter *bg-color* (color :r #x53 :g #x4e :b #x15))
  )

(clear-and-set-global-assets)

(defun draw-interior-room ()
  (let* ((base-color '(90 90 90))
         (wall-color (mapcar (λ α → (round (* α 1.5))) base-color))
         (base-color (color :r (car base-color) :g (cadr base-color) :b (caddr base-color)))
         (wall-color (color :r (car wall-color) :g (cadr wall-color) :b (caddr wall-color)))
         (floor-color (color :r 90 :g 50 :b 50)))
    (clear-display floor-color)
    ;; Draw the back wall
    (sdl:draw-filled-polygon '(#(130 0) #(480 0) #(480 250) #(0 250))
                             :color base-color)
    ;; Draw a side wall
    (sdl:draw-filled-polygon '(#(0 0) #(130 0) #(130 250) #(0 400))
                             :color wall-color)
    ;; Draw another side wall
    (sdl:draw-filled-polygon '(#(480 0) #(480 250) #(600 400) #(600 0))
                             :color wall-color)
    ;; Draw lines to make it appear thicker
    (let ((line-color (color :r 0 :g 0 :b 0)))
      (sdl:draw-shape '(#(130 0) #(130 250) #(480 250) #(480 0)) :color line-color)
      (sdl:draw-line #(130 250) #(0 400) :color line-color :aa t)
      (sdl:draw-line #(480 250) #(600 400) :color line-color :aa t)
      (sdl:draw-line #(130 251) #(0 401) :color line-color :aa t)
      (sdl:draw-line #(480 251) #(600 401) :color line-color :aa t))
    ))

(defclass BG-Layer ()
  ((Source-image
    :accessor Source-image
    :initarg :Source-image
    :initform "some.png")
   (Scale
    :accessor Scale
    :initarg :Scale
    :initform 1)
   (GL-Texture
    :accessor GL-Texture
    :initarg :GL-Texture
    :initform nil)
   (X-size
    :accessor X-size
    :initarg :X-size
    :initform 1000)
   (Y-size
    :accessor Y-size
    :initarg :Y-size
    :initform 1000)
   (Y-offset
    :accessor Y-offset
    :initarg :Y-offset
    :initform 0))
  (:documentation "A background layer for parallax scrolling"))

(defmethod bg-scaled-width (BG-Layer)
  "Get the scaled width of the object"
  (round (* (x-size BG-Layer) (scale BG-Layer))))

(defun set-global-img (index file-path scale y-offset)
  "Load up an image unless it is already set"
  (unless (aref *bg-layers* index)
    (let* ((image (lispbuilder-sdl-image:load-image
                   (format nil "~a/~a" *asset-path* file-path)))
           ;;(image (sdl:convert-to-display-format
           ;;        :surface (lispbuilder-sdl-gfx:zoom-surface scale scale
           ;;                                                   :surface image)))
           (bg-layer (make-instance 'BG-Layer
                                    :source-image image
                                    :scale scale
                                    :gl-texture (load-a-texture (format nil "~a/~a" *asset-path* file-path))
                                    :x-size (sdl:width image)
                                    :y-size (sdl:height image)
                                    :y-offset y-offset)))
      (setf (aref *bg-layers* index) bg-layer))))

(defclass Sprite ()
  ((Source-image
    :accessor Source-image
    :initarg :Source-image
    :initform "some.png")
   (Surface-image
    :accessor Surface-image
    :initarg :Surface-image
    :initform nil)
   (GL-Texture
    :accessor GL-Texture
    :initarg :GL-Texture
    :initform nil)
   (Scale
    :accessor Scale
    :initarg :Scale
    :initform 1)
   (Old-scale
    :accessor Old-scale
    :initarg :Old-scale
    :initform 1)
   (X-loc
    :accessor X-loc
    :initarg :X-loc
    :initform 0)
   (Y-loc
    :accessor Y-loc
    :initarg :Y-loc
    :initform 400)
   (X-dir
    :accessor X-dir
    :initarg :X-dir
    :initform 0)
   (Y-dir
    :accessor Y-dir
    :initarg :Y-dir
    :initform 0)
   (Animation-cell
    :accessor Animation-cell
    :initarg :Animation-cell
    :initform 7)
   (Frame-index
    :accessor Frame-index
    :initarg :Frame-index
    :initform 0))
  (:documentation "A sprite image"))

(defmethod animation ((sprite Sprite))
  "Cycle through the cells for the sprites"
  (with-accessors ((scale Scale)
                   (old-scale Old-scale)
                   (x X-loc)
                   (y Y-loc)
                   (x-dir X-dir)
                   (y-dir Y-dir)
                   (frame-index Frame-index)
                   (animation-cell Animation-cell)
                   (surface-image Surface-image)) sprite
    (incf frame-index)
    (setf scale (max .01 (float (/ (round (* 10 (/ y 100))) 10))))
    ;; Randomly walk around a small percent of the time
    (let* ((walk? (random 100))
           (px (if (> walk? 90) (1- (random 3)) x-dir))
           (py (if (> walk? 90) (1- (random 3)) y-dir)))
      ;; Additional chance to not go up/down
      (when (> (random 100) 90) (setf py 0))
      (setf x-dir px y-dir py)
      (setf x (+ x (* 4 px)))
      (setf y (+ y (* 2 py)))
      (when (< y (* 270 (/ *resolution-height* 700))) (setf y (* 270 (/ *resolution-height* 700))))
      (when (> y (* .9 *resolution-height*)) (setf y (* .9 *resolution-height*)))
      (when (< x -1600) (setf x -1600))
      (when (> x 2600) (setf x 2600))
      (let* ((direction
              (apply (λ -1 → "left" 1 → "right" α → (apply (λ -1 → "up" 1 → "down") (list py))) (list px)))
             (animation (get-animation direction)))
        (when (> frame-index (1- (length animation)))
          (setf frame-index 0))
        (setf animation-cell (aref animation frame-index))
        ))))

(defmethod sprite-layer ((sprite Sprite))
  "Get the appropriate index layer for a sprite"
  (forest-layer (y-loc Sprite)))

(defmethod get-sprite-img ((sprite Sprite))
  "Return the surface image, unless it is not set, then set it and
return."
  (if (Surface-image Sprite)
      (Surface-image Sprite)
      (let* ((image (lispbuilder-sdl-image:load-image
                     (format nil "~a/~a" *asset-path* (Source-image Sprite))))
             (image (sdl:convert-to-display-format
                     :surface (lispbuilder-sdl-gfx:zoom-surface (Scale Sprite) (Scale Sprite)
                                                                :surface image))))
        (setf (Surface-image Sprite) image))))

(defmethod get-sprite-gl-texture ((sprite Sprite))
  "Return the gl texture, unless it is not set, then set it and
return."
  (if (GL-Texture Sprite)
      (GL-Texture Sprite)
      (let* ((image (load-a-texture (format nil "~a/~a" *asset-path* (Source-image Sprite)))))
        (setf (GL-Texture Sprite) image))))

(defun forest-sprites ()
  "Fill out the forest sprites"
  (setf *sprites*
        (loop for file in
             (ψ (mapcar #'namestring
                        (directory
                         (format nil "~a/~a" *asset-path* "24x32-sprites-by-svet/all/*.*")))
                ~".*assets/(.\*)"~ → |"\\1"|)
           collect (make-instance 'Sprite :source-image file))))

(forest-sprites)

(ƒ in-range (and (> α αb) (< α αc)) → t α → nil)

(ƒ forest-layer
   (in-range α 0 (* (/ *resolution-height* 600) 230)) → 0
   (in-range α (* (/ *resolution-height* 600) 230) (* (/ *resolution-height* 600) 280)) → 1
   (in-range α (* (/ *resolution-height* 600) 280) (* (/ *resolution-height* 600) 380)) → 2
   α → 3)

(defun draw-bg ()
  (set-global-img 0 "bg/glitch-sampler/forest/sky_p.png" .1 -400 2900)
  (set-global-img 1 "bg/glitch-sampler/forest/bg_2_p.png" .4 -50 3100)
  (set-global-img 2 "bg/glitch-sampler/forest/bg_1_p.png" .5 -280 2600)
  (set-global-img 3 "bg/glitch-sampler/forest/middleground_p.png" 1 -950 2900)
  (set-global-img 4 "bg/glitch-sampler/forest/middleplus_p.png" .8 500 2200)
  (set-global-img 5 "bg/glitch-sampler/forest/foreground_p.png" 1.2 -220 2400)
  (let ((x (* *x* -2))
        (sprites-p nil)
        (x-divisor 8)
        (player-layer (forest-layer *y*))
        (layers (remove nil *bg-layers*)))
    (loop for layer across layers
       for lc from 0
       do (progn
            (sdl:draw-surface-at (source-image layer)
                                 (vector (round (/ x x-divisor))
                                         (y-offset layer)))
            (sdl:draw-surface-at (source-image layer)
                                 (vector (round (/ (- x (bg-scaled-width layer)) (max 1 x-divisor)))
                                         (y-offset layer)))
            (sdl:draw-surface-at (source-image layer)
                                 (vector (round (/ (+ x (bg-scaled-width layer)) (max 1 x-divisor)))
                                         (y-offset layer)))
            ;; Add a transparent fill to simulate darkness
            (when (< lc 4)
              (lispbuilder-sdl-gfx:draw-filled-polygon '(#(0 0) #(800 0) #(800 600) #(0 600))
                                                       :color (color :r 0 :g 0 :b 0 :a 75)))
            (setf x-divisor (/ x-divisor 2))
            (loop for sprite in *sprites*
               when (eq lc (sprite-layer sprite))
               do (progn
                    (animation sprite)
                    (sdl:draw-surface-at (get-sprite-img sprite)
                                         (vector (round (/ (+ x (x-loc sprite)) (max 1 x-divisor)))
                                                 (y-loc sprite))
                                         :cell (animation-cell sprite))))
            (when (and (not sprites-p) ;; Draw the player sprite in the appropriate z axis
                       (eq player-layer lc))
              (setf sprites-p t)
              (sdl-animation))))
    (unless sprites-p (sdl-animation))
    ))

(defun sdl-animation ()
  (setf *sprite-scale* (max .01 (float (/ (round (* 10 (/ *y* 100))) 10))))
  (let* ((spw (max 1 (floor (* *sprite-scale* 24))))
         (sph (max 1 (ceiling (* *sprite-scale* 32))))
         (rows 4)
         (cols 3))
    (unless (eq *sprite-scale* *old-sprite-scale*)
      (setf *old-sprite-scale* *sprite-scale*
            *img-player* nil))
    (unless *img-player*
      (setf *img-player* (lispbuilder-sdl-image:load-image
                          (format nil "~a/24x32-sprites-by-svet/Heroes/Fighter-F-01.png" *asset-path*)
                          :color-key-at #(0 0)))
      (setf *img-player* (sdl:convert-to-display-format
                          :surface (lispbuilder-sdl-gfx:zoom-surface *sprite-scale* *sprite-scale*
                                                                     :surface *img-player*)))
      (setf *cells* (loop for y from 0 to (* sph (1- rows)) by sph
                       append (loop for x from 0 to (* spw (1- cols)) by spw
                                 collect (list x y spw sph))))
      (setf (sdl:cells *img-player*) *cells*))
    (setf *x* (+ *x* (* 10 *px*)))
    (setf *y* (+ *y* (* 5 *py*)))
    (when (< *y* 100) (setf *y* 100))
    (when (> *y* 520) (setf *y* 520))
    (when (< *x* -1600) (setf *x* -1600))
    (when (> *x* 2600) (setf *x* 2600))
    (let* ((direction
            (apply (λ -1 → "left" 1 → "right" α → (apply (λ -1 → "up" 1 → "down") (list *py*))) (list *px*)))
           (animation (get-animation direction)))
      (when (> *frame-index* (1- (length animation)))
        (setf *frame-index* 0))
      (lispbuilder-sdl:draw-surface-at *img-player* `#(388 ,*y*) :cell (aref animation *frame-index*))
      (incf *frame-index*))))

(defun sdl-main ()
  (sdl:with-init ()
    (sdl:window 800 600 :title-caption "Blub"
                :fps (make-instance 'sdl:fps-fixed :target-frame-rate 20))
    (sdl-mixer:OPEN-AUDIO)
    (let ((music (sdl-mixer:load-music (format nil "~a/~a" *asset-path* "sample.mp3"))))
      (sdl-mixer:play-music music :loop t)
      ;;(setf (frame-rate) 15)
      (sdl:update-display)
      (sdl:with-events ()
        (:quit-event ()
                     (sdl-mixer:halt-music)
                     (sdl-mixer:free music)
                     (sdl-mixer:close-audio)
                     (clear-and-set-global-assets)
                     t)
        (:key-down-event ()
                         (when (key-down-p :sdl-key-escape) (push-quit-event))
                         (when (or (key-down-p :sdl-key-down)
                                   (key-down-p :sdl-key-s)) (setf *py* 1))
                         (when (or (key-down-p :sdl-key-up)
                                   (key-down-p :sdl-key-w)) (setf *py* -1))
                         (when (or (key-down-p :sdl-key-right)
                                   (key-down-p :sdl-key-d)) (setf *px* 1))
                         (when (or (key-down-p :sdl-key-a)
                                   (key-down-p :sdl-key-left)) (setf *px* -1))
                         t)
        (:key-up-event ()
                       (unless (or (key-down-p :sdl-key-down)
                                   (key-down-p :sdl-key-s)
                                   (key-down-p :sdl-key-w)
                                   (key-down-p :sdl-key-up)) (setf *py* 0))
                       (unless (or (key-down-p :sdl-key-left)
                                   (key-down-p :sdl-key-a)
                                   (key-down-p :sdl-key-d)
                                   (key-down-p :sdl-key-right)) (setf *px* 0))
                       t)
        (:mouse-motion-event (:X mouse-x :Y mouse-y)
                             (clear-display *bg-color*)
                             (draw-box-* (- mouse-x (/ 50 2)) (- mouse-y (/ 50 2)) 50 50 :color *bg-color*))
        (:idle ()
               (clear-display *bg-color*)
               (draw-bg)
               (sdl:update-display))
        (:video-expose-event () (sdl:update-display))))))

(defmacro restartable (&body body)
  "Helper macro since we use continue restarts a lot
[remember to hit C in slime or pick the restart so errors don't kill the app]"
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))

(defun load-a-texture (filename)
  "Read a texture (image file) and load it as an OpenGL texture"
  (let* ((texture (car (gl:gen-textures 1)))
         (image (sdl-image:load-image filename)))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :generate-mipmap t)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (sdl-base::with-pixel (pix (sdl:fp image))
      ;; Only 24 or 32 bit images work, if this errors out convert it
      (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
                              (1 :luminance)
                              (2 :luminance-alpha)
                              (3 :rgb)
                              (4 :rgba))))
        (assert (and (= (sdl-base::pixel-pitch pix)
                        (* (sdl:width image) (sdl-base::pixel-bpp pix)))
                     (zerop (rem (sdl-base::pixel-pitch pix) 4))))
        (gl:tex-image-2d :texture-2d 0 :rgba
                         (sdl:width image) (sdl:height image)
                         0
                         texture-format
                         :unsigned-byte (sdl-base::pixel-data pix))))
    texture))

(defun my-rectangle (&key (texcoords '(0 0 1 1)))
  (gl:with-primitive :quads
    (gl:tex-coord (elt texcoords 0) (elt texcoords 3))
    (gl:vertex -1 -1 0)
    (gl:tex-coord (elt texcoords 2) (elt texcoords 3))
    (gl:vertex 1 -1 0)
    (gl:tex-coord (elt texcoords 2) (elt texcoords 1))
    (gl:vertex 1 1 0)
    (gl:tex-coord (elt texcoords 0) (elt texcoords 1))
    (gl:vertex -1 1 0)))

(defparameter *gl-sprite-cells* nil)

(defun set-sprite-gl-coords ()
  "Create a cell based sprite sheet off sprites and return cropped result"
  (let ((width 153)
        (height 128)
        (w 24)
        (h 32)
        (rows 4)
        (cols 3))
    (loop for y from 0 to (1- (* rows h)) by h
       append
         (loop for x from 0 to (1- (* cols w)) by w
            collect (mapcar #'float
                            (list (+ .01 (/ x width))                 ;; Start Left
                                  (/ y height)                        ;; Start Top
                                  (- (+ (/ x width) (/ w width)) .01) ;; Width
                                  (+ (/ y height) (/ h height)))))))) ;; Height

(defun get-sprite-gl-coords (cell)
  "Pull out the appropriate sprite area"
  (if *gl-sprite-cells* (nth cell *gl-sprite-cells*)
      (progn
        (setf *gl-sprite-cells* (set-sprite-gl-coords))
        (get-sprite-gl-coords cell))))

(defun get-sprite-gl-coords-portrait ()
  "Pull out the appropriate sprite area for the portrait"
  (list .5 .02 .95 .55))

(defun get-nearest-sprite (x y range)
  "Loop across and pull nearest sprite"
  (loop for sprite in *sprites*
       when (and (in-range (x-loc sprite) (- x range) (+ x range))
                 (in-range (y-loc sprite) (- y range) (+ y range)))
       collect sprite))

(defun gl-walk ()
  (setf *sprite-scale* (max .01 (float (/ (round (* 10 (/ *y* 100))) 10))))
  (setf *x* (+ *x* (* 20 *px*)))
  (setf *y* (+ *y* (* 10 *py*)))
  (when (< *y* (* 270 (/ *resolution-height* 700))) (setf *y* (* 270 (/ *resolution-height* 700))))
  (when (> *y* (* .9 *resolution-height*)) (setf *y* (* .9 *resolution-height*)))
  (when (< *x* -1600) (setf *x* -1600))
  (when (> *x* 2600) (setf *x* 2600))
  (let* ((direction
          (apply (λ -1 → "left" 1 → "right" α → (apply (λ -1 → "up" 1 → "down") (list *py*))) (list *px*)))
         (animation (get-animation direction)))
    (when (> *frame-index* (1- (length animation)))
      (setf *frame-index* 0))
    (setf *active-cell* (aref animation *frame-index*))
    (incf *frame-index*)))

(defparameter *resolution-width* 1000)
(defparameter *resolution-height* 700)

(defun set-forest-layers ()
  "Set up the 7 parallax scroll layers"
  (setf *bg-layers* (make-array 7 :initial-element nil))
  (set-global-img 0 "bg/glitch-sampler/forest/sky.png" .8 -680)
  (set-global-img 1 "bg/glitch-sampler/forest/bg_2.png" .8 -480)
  (set-global-img 2 "bg/glitch-sampler/forest/bg_1.png" 1.2 -950)
  (set-global-img 3 "bg/glitch-sampler/forest/middleground.png" 2 -1200)
  (set-global-img 4 "bg/glitch-sampler/forest/middleplus.png" 1.6 800)
  (set-global-img 5 "bg/glitch-sampler/forest/foreground.png" 2.4 0))

(defun set-abbey-layers ()
  "Set up the 7 parallax scroll layers"
  (setf *bg-layers* (make-array 7 :initial-element nil))
  (set-global-img 0 "bg/glitch-sampler/abbey/sky.png" 1 0)
  (set-global-img 1 "bg/glitch-sampler/abbey/bg_2.png" 1 0)
  (set-global-img 2 "bg/glitch-sampler/abbey/bg_1.png" 1 0)
  (set-global-img 3 "bg/glitch-sampler/abbey/middleground.png" 1 0)
  (set-global-img 4 "bg/glitch-sampler/abbey/middleplus.png" 1 0)
  ;(set-global-img 5 "bg/glitch-sampler/abbey/foreground.png" 2.4 0))
  )

(defparameter *vecto-star* nil)
(defparameter *left-portrait* nil)
(defparameter *right-portrait* nil)

(defun set-large-portrait ()
  "Set the portrait for player chats"
  (setf *left-portrait* (load-a-texture "~/src/lisp/sdl-blub/assets/portraits/Fighter-M-01-l.png"))
  (setf *right-portrait* (load-a-texture "~/src/lisp/sdl-blub/assets/portraits/Fighter-M-01-r.png")))

;; Macro courtesy of http://3bb.cc/tutorials/cl-opengl/textures-part-4.html
(defmacro with-vecto-canvas-as-texture ((width height) &body body)
  (let ((texture (gensym "TEXTURE-")))
    `(vecto:with-canvas (:width ,width :height ,height)
       ;; run some vecto code
       ,@body
       ;; and load the result into a texture
       (let ((,texture (car (gl:gen-textures 1))))
         (gl:bind-texture :texture-2d ,texture)
         (gl:tex-parameter :texture-2d :texture-min-filter :linear)
         (gl:tex-parameter :texture-2d :generate-mipmap t)
         (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
         (gl:tex-image-2d :texture-2d 0 :rgba 300 300
                          0 :rgba :unsigned-byte
                          (vecto::image-data vecto::*graphics-state*))
         ,texture))))

(defun vecto-star ()
  (setf *vecto-star*
        (with-vecto-canvas-as-texture (300 300)
          (let ((size 100)
                (angle 0)
                (step (* 2 (/ (* pi 2) 5))))
            (vecto:translate size size)
            (vecto:move-to 0 size)
            (dotimes (i 5)
              (setf angle (+ angle step))
              (vecto:line-to (* (sin angle) size)
                             (* (cos angle) size)))
            (vecto:even-odd-clip-path)
            (vecto:end-path-no-op)
            (flet ((circle (distance)
                     (vecto:set-rgba-fill distance 0 0
                                          (- 1.0 distance))
                     (vecto:centered-circle-path 0 0 (* size distance))
                     (vecto:fill-path)))
              (loop for i downfrom 1.0 by 0.05
                 repeat 20 do
                   (circle i)))))))

(defparameter *words-texture* nil)
(defparameter *words* nil)
(defparameter *words-time* 0)

(defun say (words)
  "Say something, but don't do this more than once a second"
  (unless (or (equal *words* words)
              (> 1 (- (get-universal-time) *words-time*)))
    (setf *words-texture*
          (with-vecto-canvas-as-texture (300 300)
            (let ((font (vecto:get-font (format nil "~a/~a" *asset-path* "fonts/kenpixel.ttf"))))
              (vecto:set-rgba-fill 1 1 1 .9)
              (vecto:set-font font 8)
              (vecto:draw-string 0 0 words))))
    (setf *words* words
          *words-time* (get-universal-time))))

(defun draw ()
  "Draw a frame"
  (gl:clear :color-buffer-bit)
  (gl:color 1 1 1)
  (let ((x-divisor 8))
    (loop for layer across (remove nil *bg-layers*)
       for lc from 0
       do (progn
            (gl:with-pushed-matrix ;; The backgrounds
              (with-accessors ((gl-texture GL-Texture)
                               (scale Scale)
                               (x-size X-size)
                               (y-size Y-size)
                               (y-offset Y-offset)) layer
                (gl:translate (* -2 (/ *x* *resolution-width* .5 x-divisor))
                              (- (/ y-offset *resolution-height*))
                              0)
                (gl:scale (* scale (/ x-size *resolution-width*))
                          (* scale (/ y-size *resolution-height*))
                          0)
                (gl:bind-texture :texture-2d gl-texture)
                (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
                (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
                (setf x-divisor (/ x-divisor 2))
                (my-rectangle)))

            (loop for sprite in *sprites*
               when (eq lc (sprite-layer sprite))
               do (progn
                    (animation sprite)
                    (gl:with-pushed-matrix ;; The sprite guys
                      (with-accessors ((x-loc X-loc)
                                       (y-loc Y-loc)
                                       (animation-cell Animation-cell)
                                       (scale Scale)
                                       (gl-texture GL-Texture)) sprite
                        (let ((x-coord (round (/ (+ (* -2 *x*) (* 2 x-loc)) (max 1 x-divisor) .5)))
                              (y-coord y-loc))
                          (gl:translate (/ x-coord *resolution-width*)
                                        (- 1 (/ y-coord *resolution-height* .5))
                                        0)
                          (gl:scale (* scale (/ 20 *resolution-width*))
                                    (* scale (/ 32 *resolution-height*))
                                    0)
                          (gl:bind-texture :texture-2d (get-sprite-gl-texture sprite))
                          (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
                          (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
                          (my-rectangle :texcoords (get-sprite-gl-coords animation-cell)))))))
            (when (eq lc (forest-layer *y*))
              (gl:with-pushed-matrix ;; The player sprite
                (gl:translate 0
                              (- 1 (/ *y* *resolution-height* .5)) 0)
                (gl:scale (* *sprite-scale* (/ 20 *resolution-width*))
                          (* *sprite-scale* (/ 32 *resolution-height*)) 0) ;; How large it is

                ;; Spin in a circle
                (flet ((little-star ()
                         (gl:with-pushed-matrix
                           (gl:scale .51 .51 0)
                           (gl:translate .3 -.5 0)
                           (gl:translate (* 2.0 (sin (/ (sdl:sdl-get-ticks) 500.0)))
                                         (* .2 (cos (/ (sdl:sdl-get-ticks) 500.0)))
                                         0)
                           ;; Draw a little star
                           (gl:bind-texture :texture-2d *vecto-star*)
                           (my-rectangle)
                           (gl:with-pushed-matrix
                             (gl:rotate (/ (sdl:sdl-get-ticks) 5.0) 0 0 1)
                             (my-rectangle)
                             (gl:rotate (/ (sdl:sdl-get-ticks) 5.0) 0 0 1)
                             (my-rectangle)
                             ))))

                  ;; Star in back
                  (when (> (cos (/ (sdl:sdl-get-ticks) 500.0)) 0) (little-star))
                  (gl:bind-texture :texture-2d *player-texture*)
                  (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
                  (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
                  (my-rectangle :texcoords (get-sprite-gl-coords *active-cell*))
                  ;; Star in front
                  (when (< (cos (/ (sdl:sdl-get-ticks) 500.0)) 0) (little-star))
                  )
                )
              ))))
  (let ((speaker (car (get-nearest-sprite *x* *y* 100))))
    (when speaker
      (say (format nil "I'm at x: ~a y: ~a, you are at x: ~a y: ~a"
                   (x-loc speaker) (y-loc speaker) *x* *y*))
      (gl:with-pushed-matrix ;; Display the text being said
        (gl:translate .35 .2 0)
        (gl:bind-texture :texture-2d *words-texture*)
        (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
        (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
        (my-rectangle))
      (gl:with-pushed-matrix ;; Draw the speaker portrait
        (gl:translate -.80 -.75 0)
        (gl:scale .1 .15 0)
        (gl:bind-texture :texture-2d (gl-texture speaker))
        (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
        (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
        (my-rectangle :texcoords (get-sprite-gl-coords-portrait)))
      (gl:with-pushed-matrix ;; Draw the big avatar
        (gl:translate -.7 -.55 0)
        (gl:scale .5 1 0)
        (gl:bind-texture :texture-2d *left-portrait*)
        (my-rectangle))
      (gl:with-pushed-matrix ;; Draw the other big avatar
        (gl:translate .7 -.55 0)
        (gl:scale .5 1 0)
        (gl:bind-texture :texture-2d *right-portrait*)
        (my-rectangle))
      ))
  (gl:flush)
  (sdl:update-display))

(defun init ()
  (ψ *sprites* α → (setf (GL-Texture α) nil)) ;; Clear sprite textures
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :texture-2d)
  (vecto-star)
  (set-large-portrait)
  (say "Welcome...")
  (set-forest-layers))

(defparameter *active-cell* 7)
(defparameter *player-texture* nil)

(defun opengl-main ()
  "Test drawing with opengl"
  ;; Tutorial found at http://3bb.cc/tutorials/cl-opengl/
  (sdl:with-init ()
    (sdl:window *resolution-width* *resolution-height*
                :title-caption "Blub Game!"
                :flags sdl:sdl-opengl
                :opengl t
                :fps (make-instance 'sdl:fps-fixed :target-frame-rate 20))
    (sdl-mixer:OPEN-AUDIO)
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    ;; Texture fails beyond 2000 resolution on laptop for some reason (may need to resize)
    (let ((*player-texture* (load-a-texture "~/src/lisp/sdl-blub/assets/24x32-sprites-by-svet/all/Mage-F-01.png"))
          (music (sdl-mixer:load-music (format nil "~a/~a" *asset-path* "sample.mp3"))))
      (init)
      (sdl-mixer:play-music music :loop t)
      (sdl:with-events ()
        (:quit-event ()
                     ;; Moved this to end outside function loop
                     ;(sdl-mixer:halt-music)
                     ;(sdl-mixer:free music)
                     ;(sdl-mixer:close-audio)
                     t)
        (:key-down-event ()
                         (when (key-down-p :sdl-key-escape) (push-quit-event))
                         (when (or (key-down-p :sdl-key-down)
                                   (key-down-p :sdl-key-s)) (setf *py* 1))
                         (when (or (key-down-p :sdl-key-up)
                                   (key-down-p :sdl-key-w)) (setf *py* -1))
                         (when (or (key-down-p :sdl-key-right)
                                   (key-down-p :sdl-key-d)) (setf *px* 1))
                         (when (or (key-down-p :sdl-key-a)
                                   (key-down-p :sdl-key-left)) (setf *px* -1))
                         t)
        (:key-up-event ()
                       (unless (or (key-down-p :sdl-key-down)
                                   (key-down-p :sdl-key-s)
                                   (key-down-p :sdl-key-w)
                                   (key-down-p :sdl-key-up)) (setf *py* 0))
                       (unless (or (key-down-p :sdl-key-left)
                                   (key-down-p :sdl-key-a)
                                   (key-down-p :sdl-key-d)
                                   (key-down-p :sdl-key-right)) (setf *px* 0))
                       t)
        (:idle ()
               ;; this lets slime keep working while the main loop is running
               ;; in sbcl using the :fd-handler swank:*communication-style*
               ;; [something similar might help in other lisps]
               (gl-walk)
               #+(and sbcl (not sb-thread)) (restartable
                                              (sb-sys:serve-all-events 0))
               (restartable (draw))))
      ;; Release textures
      (gl:delete-textures
       (append
        (list *player-texture* *words-texture* *vecto-star*)
        (mapcar (λ α → (GL-Texture α)) *sprites*)
        (loop for l across (remove nil *bg-layers*) collect (GL-Texture l))))
      ;; Release audio if it was missed in quit event
      (sdl-mixer:halt-music)
      (sdl-mixer:free music)
      (sdl-mixer:close-audio)
      ;; Clear out sprites to make sure they reload textures
      (ψ *sprites* α → (setf (GL-Texture α) nil))
      )))
