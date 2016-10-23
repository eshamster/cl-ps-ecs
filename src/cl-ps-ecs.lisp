(defpackage cl-ps-ecs
  (:use :cl-ps-ecs.ecs)
  (:export :includes-all-component-types
           :ecs-component
           
           :ecs-entity
           :ecs-entity-p
           :ecs-entity-id
           :ecs-entity-parent
           :get-ecs-component
           :with-ecs-components
           :add-ecs-entity
           :delete-ecs-entity
           :do-ecs-entities
           :find-a-entity
           :find-the-entity
           :add-ecs-component
           :add-ecs-component-list
           :remove-ecs-component

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
           :ecs-main))
(in-package :cl-ps-ecs)
