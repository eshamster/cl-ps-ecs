(defpackage cl-ps-ecs
  (:use :cl-ps-ecs.ecs)
  (:export :includes-all-component-types
           :ecs-component
           
           :make-ecs-entity
           :ecs-entity
           :ecs-entity-p
           :ecs-entity-id
           :ecs-entity-parent
           :get-ecs-component
           :with-ecs-components
           :add-ecs-entity
           :add-ecs-entity-to-buffer
           :delete-ecs-entity
           :do-ecs-entities
           :find-a-entity
           :find-the-entity
           :add-ecs-component
           :add-ecs-component-list
           :delete-ecs-component-type

           :add-entity-tag
           :has-entity-tag
           :check-entity-tags
           :delete-entity-tag
           :find-a-entity-by-tag
           :do-tagged-ecs-entities
           
           :ecs-system
           :ecs-system-target-entities
           :target-entities
           :target-component-types
           :process
           :process-all
           :add-entity-hook
           :delete-entity-hook

           :do-ecs-components-of-entity           
           :register-ecs-system
           :register-next-frame-func
           :register-func-with-pred
           :ecs-main))
(in-package :cl-ps-ecs)
