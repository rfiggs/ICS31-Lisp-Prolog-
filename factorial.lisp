(DEFUN FACTORIAL (X)
    (COND
        ((OR (NOT (NUMBERP X)) (< X 0))
            NIL
        )
        ((<= X 1)
            1
        )
        (T
            (* X (FACTORIAL (- X 1)))
        )
    )
)
