00;Muhammad Mustafa
;100823576
;(make-node x): returns a value for x
;(make-edge x y): makes a pair of the arguments for x y
;(make-graph): is an object that stores edges and their nodes
;functions (add-node x), (add-edge x), (delete-node x), (delete-edge x)
;This question is written in Scheme R5RS

;returns the value passed in for x
(define (make-node x)x)

;creates a pair of nodes and returns the paired nodes' values
(define (make-edge x y)
  (list x y))

(define (make-graph)
  (let ((nodes 'empty)
        (edges 'empty)
        (adlist 'empty))
    
    ;check if node/edge exists in the graph
    (define (find-node n nlist)
      (cond ((eq? nlist '()) #f)
            ((eq? (car nlist) n) #t)
            (else (find-node n (cdr nlist)))))
    
    ;add a new node/edge to a either list
    (define (add2end n nlist)
      (if (eq? nlist '()) (list n)
          (cons (car nlist) (add2end n (cdr nlist)))))
    
    ;delete an element from list elist
    (define (dedge e elist)
      (cond ((eq? elist '()) (list e))
            ((eq? (car elist) e) (cdr elist))
            (else (cons (car elist) (dedge e (cdr elist))))))
    
    ;delete edges from list, given a node
    (define (dne n e)
      (cond ((eq? e '()) edges)
            ((eq? n (car (car e))) (begin (set! edges (dedge (car e) edges))
                                          (dne n edges)))
            ((eq? n (car (cdr (car e)))) (begin (set! edges (dedge (car e) edges))
                                                (dne n edges)))
            (else (dne n (cdr e)))))

    ;delete node from list, given a node
    (define (del-node n)
      (begin (set! nodes (dedge n nodes))))
    
    ;add a node
    (define (add-node x)
      (cond ((eq? nodes 'empty) (begin (set! nodes (list x))))
            ((find-node x nodes) (begin (display "node already exists\n")))
            (else (begin (set! nodes (add2end x nodes))))))
    
    ;add an edge
    (define (add-edge e)
      (if (find-node (car e) nodes)
          (if (find-node (car (cdr e)) nodes)
              (cond ((eq? edges 'empty) (set! edges (list e)))
                    ((find-node e edges) (display "Egde already exists\n"))
                    (else (begin (set! edges (add2end e edges)))))
              (display "second node does not exist in graph\n"))
          (display "first node does not exist in graph\n")))
    
    ;delete an edge from a graph
    (define (delete-edge e)
      (cond ((eq? edges 'empty) (display "No edges in the graph\n"))
            ((eq? (find-node e edges) #f) (display "edge does not exist\n"))
            (else (begin (display "deleting edge\n") (set! edges (dedge e edges))))))
    
    ;delete the node
    (define (delete-node n)
      (cond ((eq? nodes 'empty) (display "No nodes in the graph\n"))
            ((eq? (find-node n nodes) #f) (display "node does not exist in the graph\n"))
            (else (begin (dne n edges) (del-node n)))))
    
    ;print the graph as adjacency list
    (define (build-elist n e)
      (cond ((eq? n '()) '())
            ((eq? e '()) '())
            ((eq? n (car (car e))) (cons (car (cdr (car e))) (build-elist n (cdr e))))
            (else (build-elist n (cdr e)))))

    (define (build-nelist n e)
      (cond ((eq? n '()) '())
            ((eq? e '()) '())
            (else (list n (build-elist n e)))))

    (define (build-elist1 n)
      (build-nelist n edges))                

    (define (print-graph)
      (print-help nodes edges))
    
    (define (print-help n e)
      (cond ((eq? n '()) '())
            ((eq? e '()) '())
            (else (cons (build-elist1 (car n)) (print-help (cdr n) e)))))
    
    ;test functions
    (define (print-nodes)
      nodes)
    
    (define (print-edges)
      edges)
    
    ;dispatch a requested method
    (define (dispatch method)
      (cond ((eq? method 'add-node) add-node)
            ((eq? method 'add-edge) add-edge)
            ((eq? method 'delete-edge) delete-edge)
            ((eq? method 'delete-node) delete-node)
            ((eq? method 'print-graph) print-graph)
            ((eq? method 'print-nodes) print-nodes)
            ((eq? method 'print-edges) print-edges)
            ((eq? method 'build-elist) build-elist1)
            (else (display "unknown request\n"))))
    dispatch))
    
  

;-------------------Test Cases--------------------
(display "TEST CASES:")
(define g (make-graph))
(define n1 (make-node 1))
(define n2 (make-node 3))
(define n3 (make-node 5))
(define n4 (make-node 7))
(define n5 (make-node 9))
(define e1 (make-edge n1 n2))
(define e2 (make-edge n2 n3))
(define e3 (make-edge n3 n4))
(define e4 (make-edge n4 n5))
(define e5 (make-edge n5 n2))
(newline)
(display"-----------------")
(newline)
(display "Nodes used for test cases:\n")
(display "(define n1 (make-node 1))\n")
(display "(define n1 (make-node 3))\n")
(display "(define n1 (make-node 5))\n")
(display "(define n1 (make-node 7))\n")
(display "(define n1 (make-node 9))\n")
(newline)
(display "Edges used for test cases:\n")
(display "(define e1 (make-edge n1 n2))\n")
(display "(define e2 (make-edge n2 n3))\n")
(display "(define e3 (make-edge n3 n4))\n")
(display "(define e4 (make-edge n4 n5))\n")
(display "(define e5 (make-edge n5 n2))\n")
(newline)
(display "Graph used:\n")
(display "(define g (make-graph)\n")
(newline)

;Test cases for adding nodes and edges
((g 'add-node) n1)
((g 'add-node) n2)
((g 'add-node) n3)
((g 'add-node) n4)
((g 'add-node) n5)
((g 'add-edge) e1)
((g 'add-edge) e2)
((g 'add-edge) e3)
((g 'add-edge) e4)
((g 'add-edge) e5)

;display nodes and edges, expected output;
;(1 3 5 7 9)
;((1 3) (3 5) (5 7) (7 9) (9 3))
(display "nodes n1-n5 added, edges e1-e5 added:\n")
((g 'print-nodes))
((g 'print-edges))
(newline)

(display "testing for adding duplicate nodes and edges:\n")
;adding duplicate node, expected output is Node already exists
((g 'add-node) n1)
;adding duplicate edge, expected output is edge already exists
((g 'add-edge) e1)
(newline)

;test to print full graph, expected output is: ((1 (3)) (3 (5)) (5 (7)) (7 (9)) (9 (3)))
(display "printing graph:\n")
((g 'print-graph))
(newline)

;test for deleting node n4 expected output is: ((1 (3)) (3 (5)) (5 ()) (9 (3)))
(display "((g 'delete-node) n4):\n")
((g 'delete-node) n4)
((g 'print-graph))
(newline)

;test for deleting edge e1, expected output is: ((1 ()) (3 (5)) (5 ()) (9 (3)))
(display "((g 'delete-edge) e1):\n")
((g 'delete-edge) e1)
((g 'print-graph))