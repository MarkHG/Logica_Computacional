(**  Sistema de Tácticas 
	 Hernández Chávez Jorge Argenis
	 Hurtado Gutiérrez Marco Antonio **)


Variables  p q r s t u w:Prop.
Parameters (A:Type)
           (b:Prop)
           (P Q :Prop -> Prop)
           (f g:Prop -> Prop).

Theorem Te1: (~p -> q /\ r) /\ (~p \/ s -> ~t) /\ (u /\ ~p) -> (u /\ r) /\ ~t.
Proof.
intro.
split.
destruct H.
destruct H0.
destruct H1.
split.
trivial.
apply H.
trivial.
destruct H.
destruct H0.
apply H0.
left.
destruct H1.
trivial.
Qed.

Theorem Te2: (p -> q -> r) -> (p -> q) -> (p -> r).
Proof.
intro.
intro.
intro.
assert (q -> r).
apply H.
trivial.
apply H2.  
apply H0.
trivial.
Qed.

Theorem Te3: (forall x, Q x /\ (exists y, P y -> Q (f x))) /\
                    (forall z, Q (f z) -> Q (g z)) ->
                    P b -> exists w, Q (g w).
Proof.
intro.
intro.
destruct H.
assert (Q (g u)).
apply H1.
apply H.
exists u.
trivial.
Qed.
