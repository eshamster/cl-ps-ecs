(defpackage cl-ps-ecs
  (:use :cl-ps-ecs.ecs)
  (:export :includes-all-component-types
           :ecs-component
           
           :ecs-entity
           :ecs-entity-p
           :ecs-entity-parent
           :get-ecs-component
           :with-ecs-components
           :add-ecs-entity
           :delete-ecs-entity
           :do-ecs-entities
           :find-a-entity
           :find-the-entity
           :add-ecs-component
           :remove-ecs-component
           
           :ecs-system
           :target-component-types
           :process
           :add-entity-hook
           :delete-entity-hook

           :do-ecs-components-of-entity           
           :register-ecs-system
           :ecs-main))
(in-package :cl-ps-ecs)
