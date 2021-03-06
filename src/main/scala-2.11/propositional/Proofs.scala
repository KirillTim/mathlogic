package propositional

import propositional.ExprTypes._

object Proofs {

  def contraposition(a: Expr, b: Expr) = List[Expr](
    (!!(b) ->: (a ->: !!(b))) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a))),
    ((!!(b) ->: (a ->: !!(b))) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a)))) ->: ((a ->: b) ->: ((!!(b) ->: (a ->: !!(b))) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a))))),
    (a ->: b) ->: ((!!(b) ->: (a ->: !!(b))) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a)))),
    !!(b) ->: (a ->: !!(b)),
    (!!(b) ->: (a ->: !!(b))) ->: ((a ->: b) ->: (!!(b) ->: (a ->: !!(b)))),
    (a ->: b) ->: (!!(b) ->: (a ->: !!(b))),
    ((a ->: b) ->: (!!(b) ->: (a ->: !!(b)))) ->: (((a ->: b) ->: ((!!(b) ->: (a ->: !!(b))) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a))))) ->: ((a ->: b) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a))))),
    ((a ->: b) ->: ((!!(b) ->: (a ->: !!(b))) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a))))) ->: ((a ->: b) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a)))),
    (a ->: b) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a))),
    (!!(b) ->: (a ->: b)) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a)))),
    ((!!(b) ->: (a ->: b)) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a))))) ->: ((a ->: b) ->: ((!!(b) ->: (a ->: b)) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a)))))),
    (a ->: b) ->: ((!!(b) ->: (a ->: b)) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a))))),
    (a ->: b) ->: (!!(b) ->: (a ->: b)),
    ((a ->: b) ->: (!!(b) ->: (a ->: b))) ->: (((a ->: b) ->: ((!!(b) ->: (a ->: b)) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a)))))) ->: ((a ->: b) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a)))))),
    ((a ->: b) ->: ((!!(b) ->: (a ->: b)) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a)))))) ->: ((a ->: b) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a))))),
    (a ->: b) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a)))),
    ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))),
    (((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))))) ->: ((a ->: b) ->: (((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))))),
    (a ->: b) ->: (((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))))),
    (a ->: b) ->: ((a ->: !!(b)) ->: !!(a)),
    ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))) ->: ((a ->: b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))),
    (a ->: b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))),
    ((a ->: b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (((a ->: b) ->: (((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))))) ->: ((a ->: b) ->: (!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))))),
    ((a ->: b) ->: (((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))))) ->: ((a ->: b) ->: (!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))))),
    (a ->: b) ->: (!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))),
    ((a ->: b) ->: (!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a))))) ->: (((a ->: b) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a))))) ->: ((a ->: b) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a))))), ((a ->: b) ->: ((!!(b) ->: ((a ->: b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a))))) ->: ((a ->: b) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a)))),
    (a ->: b) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a))),
    ((a ->: b) ->: (!!(b) ->: ((a ->: !!(b)) ->: !!(a)))) ->: (((a ->: b) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a)))) ->: ((a ->: b) ->: (!!(b) ->: !!(a)))),
    ((a ->: b) ->: ((!!(b) ->: ((a ->: !!(b)) ->: !!(a))) ->: (!!(b) ->: !!(a)))) ->: ((a ->: b) ->: (!!(b) ->: !!(a))),
    (a ->: b) ->: (!!(b) ->: !!(a))
  )

  def tertiumNonDatur(e: Expr) =
    List[Expr](e ->: (e V !!(e))) ++
      contraposition(e, e V !!(e)) ++
      List[Expr](!!(e V !!(e)) ->: !!(e)) ++
      List[Expr](!!(e) ->: (e V !!(e))) ++
      contraposition(!!(e), e V !!(e)) ++
      List[Expr](!!(e V !!(e)) ->: !!(!!(e))) ++
      List[Expr](
        (!!(e V !!(e)) ->: !!(e)) ->: ((!!(e V !!(e)) ->: !!(!!(e))) ->: !!(!!(e V !!(e)))),
        (!!(e V !!(e)) ->: !!(!!(e))) ->: !!(!!(e V !!(e))),
        !!(!!(e V !!(e))),
        !!(!!(e V !!(e))) ->: (e V !!(e)),
        e V !!(e)
      )

  // And
  def andFF(a: Expr, b: Expr) =
    List[Expr](
      ((a & b) ->: a) ->: (((a & b) ->: !!(a)) ->: !!(a & b)),
      (a & b) ->: a,
      ((a & b) ->: !!(a)) ->: !!(a & b),
      !!(a) ->: ((a & b) ->: !!(a)),
      !!(a),
      (a & b) ->: !!(a),
      !!(a & b)
    )

  def andFT(a: Expr, b: Expr) =
    List[Expr](
      ((a & b) ->: a) ->: (((a & b) ->: !!(a)) ->: !!(a & b)),
      (a & b) ->: a,
      ((a & b) ->: !!(a)) ->: !!(a & b),
      !!(a) ->: ((a & b) ->: !!(a)),
      !!(a),
      (a & b) ->: !!(a),
      !!(a & b)
    )

  def andTF(a: Expr, b: Expr) =
    List[Expr](
      ((a & b) ->: b) ->: (((a & b) ->: !!(b)) ->: !!(a & b)),
      (a & b) ->: b,
      ((a & b) ->: !!(b)) ->: !!(a & b),
      !!(b) ->: ((a & b) ->: !!(b)),
      !!(b),
      (a & b) ->: !!(b),
      !!(a & b)
    )

  def andTT(a: Expr, b: Expr) =
    List[Expr](
      a ->: (b ->: (a & b)),
      a,
      b,
      b ->: (a & b),
      a & b
    )


  var orFFProof = Map[String, List[Expr]]()

  // Or
  def orFF(a: Expr, b: Expr) = List(
    !!(a),
    !!(b),
    ((a V b) ->: a) ->: (((a V b) ->: !!(a)) ->: !!(a V b)),
    !!(a) ->: ((a V b) ->: !!(a)),
    (a V b) ->: !!(a),
    (a ->: a) ->: ((b ->: a) ->: ((a V b) ->: a)),
    ((a ->: a) ->: ((b ->: a) ->: ((a V b) ->: a))) ->: ((a V b) ->: ((a ->: a) ->: ((b ->: a) ->: ((a V b) ->: a)))),
    (a V b) ->: ((a ->: a) ->: ((b ->: a) ->: ((a V b) ->: a))),
    (a ->: (a ->: a)) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a)),
    ((a ->: (a ->: a)) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a))) ->: ((a V b) ->: ((a ->: (a ->: a)) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a)))),
    (a V b) ->: ((a ->: (a ->: a)) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a))),
    a ->: (a ->: a),
    (a ->: (a ->: a)) ->: ((a V b) ->: (a ->: (a ->: a))),
    (a V b) ->: (a ->: (a ->: a)),
    ((a V b) ->: (a ->: (a ->: a))) ->: (((a V b) ->: ((a ->: (a ->: a)) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a)))) ->: ((a V b) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a)))),
    ((a V b) ->: ((a ->: (a ->: a)) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a)))) ->: ((a V b) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a))),
    (a V b) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a)),
    a ->: ((a ->: a) ->: a),
    (a ->: ((a ->: a) ->: a)) ->: ((a V b) ->: (a ->: ((a ->: a) ->: a))),
    (a V b) ->: (a ->: ((a ->: a) ->: a)),
    ((a V b) ->: (a ->: ((a ->: a) ->: a))) ->: (((a V b) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a))) ->: ((a V b) ->: (a ->: a))),
    ((a V b) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a))) ->: ((a V b) ->: (a ->: a)),
    (a V b) ->: (a ->: a),
    ((a V b) ->: (a ->: a)) ->: (((a V b) ->: ((a ->: a) ->: ((b ->: a) ->: ((a V b) ->: a)))) ->: ((a V b) ->: ((b ->: a) ->: ((a V b) ->: a)))),
    ((a V b) ->: ((a ->: a) ->: ((b ->: a) ->: ((a V b) ->: a)))) ->: ((a V b) ->: ((b ->: a) ->: ((a V b) ->: a))),
    (a V b) ->: ((b ->: a) ->: ((a V b) ->: a)),
    (b ->: !!(!!(a))) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a)),
    ((b ->: !!(!!(a))) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a))) ->: ((a V b) ->: ((b ->: !!(!!(a))) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a)))),
    (a V b) ->: ((b ->: !!(!!(a))) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a))),
    (b ->: (!!(a) ->: !!(b))) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a)))),
    ((b ->: (!!(a) ->: !!(b))) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a))))) ->: ((a V b) ->: ((b ->: (!!(a) ->: !!(b))) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a)))))),
    (a V b) ->: ((b ->: (!!(a) ->: !!(b))) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a))))),
    (b ->: !!(b)) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b)))),
    ((b ->: !!(b)) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b))))) ->: ((a V b) ->: ((b ->: !!(b)) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b)))))),
    (a V b) ->: ((b ->: !!(b)) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b))))),
    !!(b) ->: (b ->: !!(b)),
    (!!(b) ->: (b ->: !!(b))) ->: ((a V b) ->: (!!(b) ->: (b ->: !!(b)))),
    (a V b) ->: (!!(b) ->: (b ->: !!(b))),
    !!(b),
    !!(b) ->: ((a V b) ->: !!(b)),
    (a V b) ->: !!(b),
    ((a V b) ->: !!(b)) ->: (((a V b) ->: (!!(b) ->: (b ->: !!(b)))) ->: ((a V b) ->: (b ->: !!(b)))),
    ((a V b) ->: (!!(b) ->: (b ->: !!(b)))) ->: ((a V b) ->: (b ->: !!(b))),
    (a V b) ->: (b ->: !!(b)),
    ((a V b) ->: (b ->: !!(b))) ->: (((a V b) ->: ((b ->: !!(b)) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b)))))) ->: ((a V b) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b)))))),
    ((a V b) ->: ((b ->: !!(b)) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b)))))) ->: ((a V b) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b))))),
    (a V b) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b)))),
    (!!(b) ->: (!!(a) ->: !!(b))) ->: (b ->: (!!(b) ->: (!!(a) ->: !!(b)))),
    ((!!(b) ->: (!!(a) ->: !!(b))) ->: (b ->: (!!(b) ->: (!!(a) ->: !!(b))))) ->: ((a V b) ->: ((!!(b) ->: (!!(a) ->: !!(b))) ->: (b ->: (!!(b) ->: (!!(a) ->: !!(b)))))),
    (a V b) ->: ((!!(b) ->: (!!(a) ->: !!(b))) ->: (b ->: (!!(b) ->: (!!(a) ->: !!(b))))),
    !!(b) ->: (!!(a) ->: !!(b)),
    (!!(b) ->: (!!(a) ->: !!(b))) ->: ((a V b) ->: (!!(b) ->: (!!(a) ->: !!(b)))),
    (a V b) ->: (!!(b) ->: (!!(a) ->: !!(b))),
    ((a V b) ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (((a V b) ->: ((!!(b) ->: (!!(a) ->: !!(b))) ->: (b ->: (!!(b) ->: (!!(a) ->: !!(b)))))) ->: ((a V b) ->: (b ->: (!!(b) ->: (!!(a) ->: !!(b)))))),
    ((a V b) ->: ((!!(b) ->: (!!(a) ->: !!(b))) ->: (b ->: (!!(b) ->: (!!(a) ->: !!(b)))))) ->: ((a V b) ->: (b ->: (!!(b) ->: (!!(a) ->: !!(b))))),
    (a V b) ->: (b ->: (!!(b) ->: (!!(a) ->: !!(b)))),
    ((a V b) ->: (b ->: (!!(b) ->: (!!(a) ->: !!(b))))) ->: (((a V b) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b))))) ->: ((a V b) ->: (b ->: (!!(a) ->: !!(b))))),
    ((a V b) ->: ((b ->: (!!(b) ->: (!!(a) ->: !!(b)))) ->: (b ->: (!!(a) ->: !!(b))))) ->: ((a V b) ->: (b ->: (!!(a) ->: !!(b)))),
    (a V b) ->: (b ->: (!!(a) ->: !!(b))),
    ((a V b) ->: (b ->: (!!(a) ->: !!(b)))) ->: (((a V b) ->: ((b ->: (!!(a) ->: !!(b))) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a)))))) ->: ((a V b) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a)))))),
    ((a V b) ->: ((b ->: (!!(a) ->: !!(b))) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a)))))) ->: ((a V b) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a))))),
    (a V b) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a)))),
    (b ->: (!!(a) ->: b)) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))),
    ((b ->: (!!(a) ->: b)) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))))) ->: ((a V b) ->: ((b ->: (!!(a) ->: b)) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))))),
    (a V b) ->: ((b ->: (!!(a) ->: b)) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))))),
    b ->: (!!(a) ->: b),
    (b ->: (!!(a) ->: b)) ->: ((a V b) ->: (b ->: (!!(a) ->: b))),
    (a V b) ->: (b ->: (!!(a) ->: b)),
    ((a V b) ->: (b ->: (!!(a) ->: b))) ->: (((a V b) ->: ((b ->: (!!(a) ->: b)) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))))) ->: ((a V b) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))))),
    ((a V b) ->: ((b ->: (!!(a) ->: b)) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))))) ->: ((a V b) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))))),
    (a V b) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))),
    ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))),
    (((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))))) ->: ((a V b) ->: (((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))))),
    (a V b) ->: (((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))))),
    (!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))),
    ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: ((a V b) ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))),
    (a V b) ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))),
    ((a V b) ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (((a V b) ->: (((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))))) ->: ((a V b) ->: (b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))))),
    ((a V b) ->: (((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))))) ->: ((a V b) ->: (b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))))),
    (a V b) ->: (b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))),
    ((a V b) ->: (b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))))) ->: (((a V b) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))))) ->: ((a V b) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))))),
    ((a V b) ->: ((b ->: ((!!(a) ->: b) ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))))) ->: ((a V b) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))),
    (a V b) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))),
    ((a V b) ->: (b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a))))) ->: (((a V b) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a))))) ->: ((a V b) ->: (b ->: !!(!!(a))))),
    ((a V b) ->: ((b ->: ((!!(a) ->: !!(b)) ->: !!(!!(a)))) ->: (b ->: !!(!!(a))))) ->: ((a V b) ->: (b ->: !!(!!(a)))),
    (a V b) ->: (b ->: !!(!!(a))),
    ((a V b) ->: (b ->: !!(!!(a)))) ->: (((a V b) ->: ((b ->: !!(!!(a))) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a)))) ->: ((a V b) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a)))),
    ((a V b) ->: ((b ->: !!(!!(a))) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a)))) ->: ((a V b) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a))),
    (a V b) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a)),
    (!!(!!(a)) ->: a) ->: (b ->: (!!(!!(a)) ->: a)),
    ((!!(!!(a)) ->: a) ->: (b ->: (!!(!!(a)) ->: a))) ->: ((a V b) ->: ((!!(!!(a)) ->: a) ->: (b ->: (!!(!!(a)) ->: a)))),
    (a V b) ->: ((!!(!!(a)) ->: a) ->: (b ->: (!!(!!(a)) ->: a))),
    !!(!!(a)) ->: a,
    (!!(!!(a)) ->: a) ->: ((a V b) ->: (!!(!!(a)) ->: a)),
    (a V b) ->: (!!(!!(a)) ->: a),
    ((a V b) ->: (!!(!!(a)) ->: a)) ->: (((a V b) ->: ((!!(!!(a)) ->: a) ->: (b ->: (!!(!!(a)) ->: a)))) ->: ((a V b) ->: (b ->: (!!(!!(a)) ->: a)))),
    ((a V b) ->: ((!!(!!(a)) ->: a) ->: (b ->: (!!(!!(a)) ->: a)))) ->: ((a V b) ->: (b ->: (!!(!!(a)) ->: a))),
    (a V b) ->: (b ->: (!!(!!(a)) ->: a)),
    ((a V b) ->: (b ->: (!!(!!(a)) ->: a))) ->: (((a V b) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a))) ->: ((a V b) ->: (b ->: a))),
    ((a V b) ->: ((b ->: (!!(!!(a)) ->: a)) ->: (b ->: a))) ->: ((a V b) ->: (b ->: a)),
    (a V b) ->: (b ->: a),
    ((a V b) ->: (b ->: a)) ->: (((a V b) ->: ((b ->: a) ->: ((a V b) ->: a))) ->: ((a V b) ->: ((a V b) ->: a))),
    ((a V b) ->: ((b ->: a) ->: ((a V b) ->: a))) ->: ((a V b) ->: ((a V b) ->: a)),
    (a V b) ->: ((a V b) ->: a),
    (a V b) ->: (((a V b) ->: (a V b)) ->: (a V b)),
    (a V b) ->: ((a V b) ->: (a V b)),
    ((a V b) ->: ((a V b) ->: (a V b))) ->: (((a V b) ->: (((a V b) ->: (a V b)) ->: (a V b))) ->: ((a V b) ->: (a V b))),
    ((a V b) ->: (((a V b) ->: (a V b)) ->: (a V b))) ->: ((a V b) ->: (a V b)),
    (a V b) ->: (a V b),
    ((a V b) ->: (a V b)) ->: (((a V b) ->: ((a V b) ->: a)) ->: ((a V b) ->: a)),
    ((a V b) ->: ((a V b) ->: a)) ->: ((a V b) ->: a),
    (a V b) ->: a,
    ((a V b) ->: !!(a)) ->: !!(a V b),
    !!(a V b)
  )

  def orFT(a: Expr, b: Expr) =
    List[Expr](
      b ->: (a V b),
      b,
      a V b
    )

  def orTF(a: Expr, b: Expr) =
    List[Expr](
      a ->: (a V b),
      a,
      a V b
    )

  def orTT(a: Expr, b: Expr) =
    List[Expr](
      a ->: (a V b),
      a,
      a V b
    )

  // Impl
  def implFF(a: Expr, b: Expr) =
    List[Expr](
      (!!(b) ->: a) ->: ((!!(b) ->: !!(a)) ->: !!(!!(b))),
      ((!!(b) ->: a) ->: ((!!(b) ->: !!(a)) ->: !!(!!(b)))) ->: (a ->: ((!!(b) ->: a) ->: ((!!(b) ->: !!(a)) ->: !!(!!(b))))),
      a ->: ((!!(b) ->: a) ->: ((!!(b) ->: !!(a)) ->: !!(!!(b)))),
      !!(a) ->: (!!(b) ->: !!(a)),
      (!!(a) ->: (!!(b) ->: !!(a))) ->: (a ->: (!!(a) ->: (!!(b) ->: !!(a)))),
      a ->: (!!(a) ->: (!!(b) ->: !!(a))),
      a ->: (!!(b) ->: a),
      (a ->: (!!(b) ->: a)) ->: (a ->: (a ->: (!!(b) ->: a))),
      a ->: (a ->: (!!(b) ->: a)),
      a ->: (a ->: a),
      (a ->: (a ->: a)) ->: ((a ->: ((a ->: a) ->: a)) ->: (a ->: a)),
      (a ->: ((a ->: a) ->: a)) ->: (a ->: a),
      a ->: ((a ->: a) ->: a),
      a ->: a,
      !!(a),
      !!(a) ->: (a ->: !!(a)),
      a ->: !!(a),
      (a ->: a) ->: ((a ->: (a ->: (!!(b) ->: a))) ->: (a ->: (!!(b) ->: a))),
      (a ->: (a ->: (!!(b) ->: a))) ->: (a ->: (!!(b) ->: a)),
      a ->: (!!(b) ->: a),
      (a ->: !!(a)) ->: ((a ->: (!!(a) ->: (!!(b) ->: !!(a)))) ->: (a ->: (!!(b) ->: !!(a)))),
      (a ->: (!!(a) ->: (!!(b) ->: !!(a)))) ->: (a ->: (!!(b) ->: !!(a))),
      a ->: (!!(b) ->: !!(a)),
      (a ->: (!!(b) ->: a)) ->: ((a ->: ((!!(b) ->: a) ->: ((!!(b) ->: !!(a)) ->: !!(!!(b))))) ->: (a ->: ((!!(b) ->: !!(a)) ->: !!(!!(b))))),
      (a ->: ((!!(b) ->: a) ->: ((!!(b) ->: !!(a)) ->: !!(!!(b))))) ->: (a ->: ((!!(b) ->: !!(a)) ->: !!(!!(b)))),
      a ->: ((!!(b) ->: !!(a)) ->: !!(!!(b))),
      (a ->: (!!(b) ->: !!(a))) ->: ((a ->: ((!!(b) ->: !!(a)) ->: !!(!!(b)))) ->: (a ->: !!(!!(b)))),
      (a ->: ((!!(b) ->: !!(a)) ->: !!(!!(b)))) ->: (a ->: !!(!!(b))),
      a ->: !!(!!(b)),
      !!(!!(b)) ->: b,
      (!!(!!(b)) ->: b) ->: (a ->: (!!(!!(b)) ->: b)),
      a ->: (!!(!!(b)) ->: b),
      (a ->: !!(!!(b))) ->: ((a ->: (!!(!!(b)) ->: b)) ->: (a ->: b)),
      (a ->: (!!(!!(b)) ->: b)) ->: (a ->: b),
      a ->: b
    )

  def implFT(a: Expr, b: Expr) =
    List[Expr](
      b ->: (a ->: b),
      b,
      a ->: b
    )

  def implTF(a: Expr, b: Expr, values: Map[String, Boolean]) =
    List[Expr](
      a,
      !!(b),
      !!(b) ->: ((a ->: b) ->: !!(b)),
      (a ->: b) ->: !!(b),
      (a ->: b) ->: (((a ->: b) ->: (a ->: b)) ->: (a ->: b)),
      (a ->: b) ->: ((a ->: b) ->: (a ->: b)),
      ((a ->: b) ->: ((a ->: b) ->: (a ->: b))) ->: (((a ->: b) ->: (((a ->: b) ->: (a ->: b)) ->: (a ->: b))) ->: ((a ->: b) ->: (a ->: b))),
      ((a ->: b) ->: (((a ->: b) ->: (a ->: b)) ->: (a ->: b))) ->: ((a ->: b) ->: (a ->: b)),
      (a ->: b) ->: (a ->: b),
      a,
      a ->: ((a ->: b) ->: a),
      (a ->: b) ->: a,
      ((a ->: b) ->: a) ->: (((a ->: b) ->: (a ->: b)) ->: ((a ->: b) ->: b)),
      ((a ->: b) ->: (a ->: b)) ->: ((a ->: b) ->: b),
      (a ->: b) ->: b,
      ((a ->: b) ->: b) ->: (((a ->: b) ->: !!(b)) ->: !!(a ->: b)),
      ((a ->: b) ->: !!(b)) ->: !!(a ->: b),
      !!(a ->: b)
    )


  def implTT(a: Expr, b: Expr) =
    List[Expr](
      b ->: (a ->: b),
      b,
      a ->: b
    )

  // Not
  def notT(a: Expr) =
    List[Expr](
      (!!(a) ->: a) ->: ((!!(a) ->: !!(a)) ->: !!(!!(a))),
      a ->: (!!(a) ->: a),
      a,
      !!(a) ->: a,
      (!!(a) ->: !!(a)) ->: !!(!!(a)),
      !!(a) ->: (!!(a) ->: !!(a)),
      (!!(a) ->: (!!(a) ->: !!(a))) ->: ((!!(a) ->: ((!!(a) ->: !!(a)) ->: !!(a))) ->: (!!(a) ->: !!(a))),
      (!!(a) ->: ((!!(a) ->: !!(a)) ->: !!(a))) ->: (!!(a) ->: !!(a)),
      !!(a) ->: ((!!(a) ->: !!(a)) ->: !!(a)),
      !!(a) ->: !!(a),
      !!(!!(a))
    )

  def notF(a: Expr) =
    List[Expr](
      !!(a)
    )


  def buildProof(e: Expr, values: Map[String, Boolean]): List[Expr] = {
    var rv = List[Expr]()
    e match {
      case a & b =>
        val l = a.evaluate(values)
        val r = b.evaluate(values)
        rv ++= buildProof(a, values)
        rv ++= buildProof(b, values)
        (l, r) match {
          case (true, true) =>
            rv ++= andTT(a, b)
          case (true, false) =>
            rv ++= andTF(a, b)
          case (false, true) =>
            rv ++= andFT(a, b)
          case (false, false) =>
            rv ++= andFF(a, b)
        }
      case a V b =>
        val l = a.evaluate(values)
        val r = b.evaluate(values)
        rv ++= buildProof(a, values)
        rv ++= buildProof(b, values)
        (l, r) match {
          case (true, true) =>
            rv ++= orTT(a, b)
          case (true, false) =>
            rv ++= orTF(a, b)
          case (false, true) =>
            rv ++= orFT(a, b)
          case (false, false) =>
            rv ++= orFF(a, b)
        }
      case a -> b =>
        val l = a.evaluate(values)
        val r = b.evaluate(values)
        rv ++= buildProof(a, values)
        rv ++= buildProof(b, values)
        (l, r) match {
          case (true, true) =>
            rv ++= implTT(a, b)
          case (true, false) =>
            rv ++= implTF(a, b, values)
          case (false, true) =>
            rv ++= implFT(a, b)
          case (false, false) =>
            rv ++= implFF(a, b)
        }
      case !!(a) =>
        rv ++= buildProof(a, values)
        if (a.evaluate(values))
          rv ++= notT(a)
        else
          rv ++= notF(a)
      case _ => //nothing
    }
    rv
  }
}
