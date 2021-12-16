# Pathfinding for Common Lisp

This is a very simple, generic implementation of the [A*][a*] pathfinding algorithm for Common Lisp. Since it's intension is to be able to handle arbitrary graphs, it has a few limitations that shouldn't be a big deal in most situations.

## Quickstart

The `pathfind` function does all the heavy lifting. It's iterative (not recursive), and so it can handle pretty large graphs if necessary.

    (pathfind start goal edges &key h limit (test 'equal))

Every path is made up of nodes. You define what a node is, but it _must be hashable_. The `test` argument is used as the `:test` parameter to the open and closed set hash tables. For example, if you were pathfinding on a 2D grid, a `node` might be a `(x y)` pair. The `start` and `goal` parameters are nodes.

The `edges` parameter is a function that - given a node - returns a list of all neighbor nodes and the cost of traversing that edge. For example (keeping with the `(x y)` node definition):

    ;; returns the list of 4 cardinal nodes adjacent to (3 3)
    CL-USER > (funcall edges '(3 3))
    (((3 2) 5.7)
     ((2 3) 9)
     ((4 3) nil)
     ((3 4) 2))

Ideally, no extra nodes are in the list of edges than necessary, but if a cost associated with the node is `nil`, then the algorithm assumes the node is not a valid edge and it is ignored.

The `h` argument is the all-important heuristic function. It is a function that - provided start and goal nodes, estimates the total cost of getting to the goal from the start. It's important that the `h` function return a cost that is an _underestimation_ of the actual cost, otherwise you can end up with non-optimal paths (this is a property of the A* algorithm and not this implementation).

The default `h` function just returns a constant cost of `0`, which treats all nodes equal and is no better than [Dijkstra][dijkstra] at that point, so it's recommended that you provide one. Keeping with the 2D grid example, this is a common `H` function:

    (defun h (start goal)
      (destructuring-bind (x1 y1) start
        (destructuring-bind (x2 y2) goal
          (let ((dx (- x2 x1))
                (dy (- y2 y1)))
            (+ (* dx dx) (* dy dy))))))

The `limit` parameter identifies the longest path allowed to be returned. This allows you to limit how much time you'll actually spent trying to find a path. The default value of `nil` means there is no limit and it will exhaust all possible paths before failing.

Upon success, the returned `#<PATH>` object contains the following accessors:

* `path-node` - the `goal` node passed to the `pathfind` function
* `path-length` - the total length of the path
* `path-cost` - the total cost of the path getting to this node
* `path-parent` - the previous `path` object in the path to this one

Often times you only care about the `path-length` and the `path-cost` of the final step in the path, which is what's returned. But, using the `path-parent` it's possible to reconstruct the entire path. The `path-list` function does this for you:

    CL-USER > (pathfind '(0 0) '(9 9) :h #'h)
    #<ASTAR::PATH of length 18 with cost 40>
    34

    CL-USER > (path-list path)
    ((0 0) (0 1) (0 2) (1 2) (2 2) (3 2)
     (4 2) (5 2) (6 2) (6 3) (7 3) (7 4)
     (7 5) (8 5) (8 6) (8 7) (8 8) (9 8)
     (9 9))

It should be noted that while the `path-length` above is 18, the returned `path-list` is 19 nodes long. This is because the `start` node - while returned in the `path-list` - doesn't require a "step" to traverse (it has a cost of zero) and so doesn't count against the length of the path.

Upon success, the second value returned is the total number of nodes searched. If no path exists from `start` to `goal` then `nil` is returned.


[a*]: https://en.wikipedia.org/wiki/A*_search_algorithm
[dijkstra]: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
