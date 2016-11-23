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

## Basic structures and functions

### `ecs-entity`

The `ecs-entity` has some members. However, these will not be set by users. The `add-ecs-entity` can set a parent of an entity by its second argument. The members, `parent` and `children`, are set by it.

```lisp
(defstruct.ps+ ecs-entity
  (id (incf *entity-id-counter*))
  (tags '())
  (components '())
  parent
  (children '())
  (registerp nil) ;; This is used only in this library.
  )
```

### `ecs-component`

The `ecs-component` has no default member.

### `ecs-system`

The main members in `ecs-system` are `target-component-types` and `process`. See the above quickstart to know about them. 

Then other important members are the followings.

- `process-all`: This is called once in `ecs-main`. The argument is the system itself.
- `target-entities`: This is automatically updated. So don't update it by manual. But reading it is useful in some cases.
- `add-entity-hook`: This is called when the system recognizes its target entity.
- `delete-entity-hook`: This is called when the system unrecognizes its target entity.

```lisp
(defstruct.ps+ ecs-system
  (enable t)
  (target-entities '()) ;; automatically updated
  (target-component-types '())
  (process (lambda (entity) entity)) ;; process each entity
  (process-all (lambda (system) system))
  (add-entity-hook (lambda (entity) entity))
  (delete-entity-hook (lambda (entity) entity)))
```

## Tag system

Tags are important to distinguish who is an entity.

- Add and delete
	- `add-entity-tag` (Ex. `(add-entity-tag entity "enemy" "slime")`)
	- `delete-entity-tag` (Ex. `(delete-entity-tag entity "slime")`)
- Check
	- `has-entity-tag` only returns whether the entity has the tag or not. (Ex. `(has-entity-tag entity "enemy")`)
	- `check-entity-tags` thorws error if the entity doesn't have the tag. (Ex. `(check-entity-tag entity "enemy")`)
- Utils
	- `find-a-entity-by-tag` finds a entity that has a specified tag from global environment. (Ex. `(find-a-entity-by-tag "slime")`)
	- `do-tagged-ecs-entities` is used for processing all entities that have a specified tag. (Ex. see below)

```lisp
(do-tagged-ecs-entities (entity "enemy")
  (print (ecs-entity-id entity)))
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
