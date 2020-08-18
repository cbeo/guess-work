# guess-work

An afternoon project to calculate the probability of events using a
monte carlo simulation and a small logic DSL.

## Example 

    > (use-package :guess-work)

To describe the space of events you need to define a situation

     > (defsituation flavors spicy sweet salty sour)
     > (flavors "pudding"
                :sweet 0.7  ; most puddings are sweet
                :salty 0.1  ; some quite horrible puddings are salty
                )
     ; #<flavors>
     
You also need to define a "rule". Here is a somewhat inaccurate rule
that describes when something tastes good:

     
    > (defvar +tasty+ 
        '(:or (:and sweet spicy) 
              (:and sweet salty) 
              (:and spicy sour) 
              (:and sweet sour)
              (:and spicy salty)))))
    
    
In general, you can use expression of nested conjunction and disjunctions, e.g. `(:and a b (:or c d))`

Next you need some situations to run the rule on.

    > (defvar +data+ 
        (list (flavors "pudding" :sweet 0.7 :salty 0.2)
              (flavors "pizzas" :sweet 0.2 :salty 0.9 :spicy 0.7)
              (flavors "ramen" :salty 0.9 :sour 0.1 :spicy 0.9)))
                   

Finally, just run some simulations:

    > (with-situation-class (flavors) 
        (let ((rule (make-rule +tasty+)))
          (loop :for situation :in +data+ 
                :do (format t "~10a: ~a~%" 
                            (slot-value situation 'label)
                            (run-simulation situation rule)))))
    pudding   : 0.14
    pizzas    : 0.7
    ramen     : 0.81
        
According to our rule, it looks like like ramen is most likely to be
tasty and pusdding is least likely to be tasty.
