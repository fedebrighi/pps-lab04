package it.unibo.pps.tasks.adts

import it.unibo.pps.u03.Sequences.Sequence

import scala.NamedTuple.Head

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    case class ComplexImpl(realPart: Double, imaginaryPart: Double)
    
    import ComplexImpl.*  
    
    // Change assignment below: should probably define a case class and use it?
    
    opaque type Complex = ComplexImpl
    
    def complex(re: Double, im: Double): Complex = ComplexImpl(re,im)
    extension (complex: Complex)
      def re(): Double = complex.realPart
      def im(): Double = complex.imaginaryPart
      def sum(other: Complex): Complex = 
        ComplexImpl(complex.realPart + other.realPart, complex.imaginaryPart + other.imaginaryPart)
      def subtract(other: Complex): Complex =
        ComplexImpl(complex.realPart - other.realPart, complex.imaginaryPart - other.imaginaryPart)
      def asString(): String =
        if complex.imaginaryPart == 0 then complex.realPart + ""
        else if complex.realPart != 0 && complex.imaginaryPart < 0 then complex.realPart + " - " + -complex.imaginaryPart + "i"
        else if complex.realPart == 0 then complex.imaginaryPart + "i"
        else complex.realPart + " + " + complex.imaginaryPart + "i"
          
