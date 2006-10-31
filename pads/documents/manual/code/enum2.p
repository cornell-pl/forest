/*@FILE @LEFT enum.tex enum-base.tex*/

/*@BEGIN enum.tex */
Penum orderStates Pprefix("S_"){
    init,
    lec = 2,
    care,
    my_for Pfrom("for") = 10,
    my_if  Pfrom("if"),
    tpv = 5
};
/*@END enum.tex */

/*@BEGIN enum-base.tex */
Penum Bool_t Pfrom (Pb_uint8) { False, True };
/*@END enum-base.tex */
