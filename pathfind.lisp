;;;; Pathfinding for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :boost-pathfind
  (:use :cl)
  (:nicknames :pf)
  (:export
   #:pathfind

   ;; path accessors
   #:path-parent
   #:path-node
   #:path-cost
   #:path-list))

(in-package :boost-pathfind)

;;; ----------------------------------------------------

(defclass path ()
  ((node   :initarg :node   :initform nil :accessor path-node)
   (parent :initarg :parent :initform nil :accessor path-parent)
   (length :initarg :length :initform 0   :accessor path-length)
   (cost   :initarg :cost   :initform 0   :accessor path-cost)
   (f      :initarg :f      :initform 0   :accessor path-f))
  (:documentation "A* pathfind solution node."))

;;; ----------------------------------------------------

(defmethod print-object ((p path) stream)
  "Output a result path."
  (print-unreadable-object (p stream :type t)
    (with-slots (length cost)
        p
      (format stream "of length ~a with cost ~a" length cost))))

;;; ----------------------------------------------------

(defmethod path-list ((path path))
  "Returns the path as a list of nodes from start to end."
  (let ((list nil))
    (do ((path path (path-parent path)))
        ((null path) list)
      (push (path-node path) list))))

;;; ----------------------------------------------------

(defun constant-zero ()
  "A default H function that returns the same estimated cost."
  #'(lambda (start goal) (declare (ignore start goal)) 0))

;;; ----------------------------------------------------

(defun pathfind (start goal edges &key (h (constant-zero)) limit (test 'equal))
  "Iteratively perform an A* pathfind."
  (let ((open-set (make-hash-table :test test))
        (closed-set (make-hash-table :test test))
        (sorted))

    ;; initialize the open set with the start
    (let ((path (make-instance 'path :node start)))
      (setf (gethash start open-set) path)

      ;; it's the best - and only - path node to search
      (push path sorted))

    ;; main algorithm
    (do ((searched 0 (1+ searched)))
        ((null sorted))

      ;; find the smallest f-score node in the open set
      (let* ((path (pop sorted))
             (node (path-node path)))

        ;; move it from the open set
        (remhash node open-set)

        ;; add it to the closed set
        (setf (gethash node closed-set) path)

        ;; was the goal reached?
        (when (equal node goal)
          (return-from pathfind (values path searched)))

        ;; find all neighbors and add them to the open set
        (when (or (null limit) (< (path-length path) limit))
          (loop
             for (n cost) in (funcall edges node)

             ;; ignore if in closed yet, update or add in open set
             unless (or (null cost)
                        (gethash n closed-set))

             ;; calculate the path cost and estimated total cost
             do (let* ((score (+ cost (path-cost path)))
                       (existing (gethash n open-set))
                       (f (+ score (funcall h n goal))))
                  (cond
                    ((not existing)
                     (let ((path (make-instance 'path
                                                :node n
                                                :parent path
                                                :cost score
                                                :f f
                                                :length (1+ (path-length path)))))
                       (setf (gethash n open-set) path)

                       ;; insert it - sorted - into the search list
                       (setf sorted (merge 'list (list path) sorted #'< :key #'path-f))))

                    ;; in the open-set, but with a better path cost
                    ((< score (path-cost existing))
                     (setf (path-parent existing) path
                           (path-cost existing) score
                           (path-f existing) f
                           (path-length existing) (1+ (path-length path)))

                     ;; re-sort the search list; TODO: once per step instead of per neighbor
                     (setf sorted (sort sorted #'< :key #'path-f)))))))))))
