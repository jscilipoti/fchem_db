module srk
    use ar_interface, only: ar_fun

    real :: a, b

contains

    subroutine ar(z, v, t)
        ...
    end subroutine

    subroutine setup(a_in, b_in)
        a = a_in
        b = b_in

        ar_fun => ar
    end subroutine
end module


program main
    use srk

    a = 2
    b = 3

    call setup(a, b)

    call TERMO(nc, MTYP, INDIC, T, P, rn, V, PHILOG, DLPHIP, DLPHIT, FUGN)
end program
