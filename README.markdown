# Cl-Ps-Ecs

`Cl-Ps-Ecs` is an entity-component-system library for Common Lisp and for Parenscript.



## Usage

### About terms

This entity-component-system consists of the following 3 elements.

- An "Entity" has only a unique ID and multiple "components".
	- In this library, it can have a parent and children.
- A "Component" is a structure that has only data.
- A "System" recognize "entities" that has specific kinds of "components", and processes the data.

### Quickstart

Define some components by inheriting `ecs-component`.

```lisp
(defstruct (vector-2d (:include ecs-component)) (x 0) (y 0))
(defstruct (position-2d (:include vector-2d)))
(defstruct (velocity-2d (:include vector-2d)))
```

Define some systems by inheriting `ecs-system` and register them. (Note that they are processed in the order of registeration.)

```lisp
(defun process-move-system (entity)
  (with-ecs-components ((pos position-2d) (vel velocity-2d)) entity
    (incf (position-2d-x pos) (velocity-2d-x vel))
    (incf (position-2d-y pos) (velocity-2d-y vel))))

(defstruct (move-system
             (:include ecs-system
                       (target-component-types '(position-2d velocity-2d))
                       (process #'process-move-system))))

(register-ecs-system :move (make-move-system))
```

Define some entities using `ecs-entity` and resiter them. (Note that systems can recognize only entities that has been registered.)

```lisp
(let ((entity (make-ecs-entity)))
  (add-ecs-component-list
   entity
   (make-position-2d :x 0 :y 0)
   (make-velocity-2d :x 1 :y 0))
  (add-ecs-entity entity))

(let ((entity (make-ecs-entity)))
  (add-ecs-component-list
   entity
   (make-position-2d :x 0 :y 0)
   (make-velocity-2d :x 0 :y -1))
  (add-ecs-entity entity))

;; Because this entity has no velosity-2d component,
;; it is not processed by the move-system.
(let ((entity (make-ecs-entity)))
  (add-ecs-component-list
   entity
   (make-position-2d :x 0 :y 0))
  (add-ecs-entity entity))
```

Run all registered systems.

```lisp
(defun print-all-entities ()
  (do-ecs-entities entity
    (with-ecs-components (position-2d) entity
      (format t "ID = ~D, pos = (~A, ~A)~%"
              (ecs-entity-id entity)
              (vector-2d-x position-2d)
              (vector-2d-y position-2d)))))

(progn (print-all-entities)
       (format t "--- Run ecs-main ---~%")
       (ecs-main)
       (print-all-entities))
```

The result is as below.

```text
ID = 3, pos = (0, 0)
ID = 2, pos = (0, 0)
ID = 1, pos = (0, 0)
--- Run ecs-main ---
ID = 3, pos = (0, 0)
ID = 2, pos = (0, -1)
ID = 1, pos = (1, 0)
```

## Installation

This library and some dependent libraries are not registered in quicklisp. So please clone them under the directory that quicklisp can find. (If you use [Roswell](https://github.com/roswell/roswell), the default is `~/.roswell/local-projects`).

- This library.
- [ps-experiment](https://github.com/eshamster/ps-experiment)

Then, load by `ql:quickload`.

```lisp
$ (ql:quickload :cl-ps-ecs)
```

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2015 eshamster (hamgoostar@gmail.com)

## License

Distributed under the LLGPL License
