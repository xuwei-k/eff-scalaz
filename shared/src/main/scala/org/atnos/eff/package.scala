package org.atnos

import org.atnos.eff.{SafeCreation, SafeInterpretation}

package object eff {

  type <=[M[_], R] = Member.<=[M, R]
  type |=[M[_], R] = MemberIn.|=[M, R]

  object eff         extends EffCreation          with EffInterpretation
  object reader      extends ReaderCreation       with ReaderInterpretation
  object writer      extends WriterCreation       with WriterInterpretation
  object state       extends StateCreation        with StateInterpretation
  object eval        extends EvalEffect
  object option      extends OptionCreation       with OptionInterpretation
  object list        extends ListCreation         with ListInterpretation
  object disjunction extends DisjunctionCreation  with DisjunctionInterpretation
  object validate    extends ValidateCreation     with ValidateInterpretation
  object choose      extends ChooseCreation       with ChooseInterpretation
  object future      extends FutureCreation       with FutureInterpretation
  object safe        extends SafeCreation         with SafeInterpretation

  object create extends
    ReaderCreation with
    WriterCreation with
    StateCreation with
    EvalCreation with
    OptionCreation with
    ListCreation with
    DisjunctionCreation with
    ValidateCreation with
    ChooseCreation with
    FutureCreation with
    SafeCreation with
    EffCreation

  object all extends
    ReaderEffect with
    WriterEffect with
    StateEffect with
    EvalEffect with
    OptionEffect with
    ListEffect with
    DisjunctionEffect with
    ValidateEffect with
    ChooseEffect with
    FutureEffect with
    SafeEffect with
    EffInterpretation with
    EffCreation with
    EffImplicits

  object interpret extends
    Interpret

}
