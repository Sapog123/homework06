package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '====', деривацию для Map Seq Option
Опционально - разработать ==== для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A): Boolean
}

object Eq {
  implicit def mapEq[A, B](implicit a: Eq[A], b: Eq[B]): Eq[Map[A, B]] = {

    def equiv(lft: Map[A, B], rgt: Map[A, B])= {
      lft.keys.zip(rgt.keys).forall(keys => a.equiv(keys._1, keys._2)) &&
        lft.values.zip(rgt.values).forall(values => b.equiv(values._1, values._2))
    }
    return  equiv
  }

  implicit def seqEq[A](implicit a: Eq[A]): Eq[Seq[A]] = {
        def equiv(lft: Seq[A], rgt: Seq[A])= {
      lft.zip(rgt).forall(v=> a.equiv(v._1,v._2))
    }
    return  equiv
  }

  implicit def optionEq[A](implicit a: Eq[A]): Eq[Option[A]] = {

    def equiv(lft: Option[A], rgt: Option[A]) = a.equiv(lft.get, rgt.get)
    return  equiv
  }

  implicit val intEq: Eq[Int] = (lft: Int, rgt: Int) => lft == rgt
  implicit val doubleEq: Eq[Double] = (lft: Double, rgt: Double) => lft == rgt

  implicit def complexEq(implicit eq: Eq[Double]): Eq[ComplexNumber] = {
    def equiv(lft: ComplexNumber, rgt: ComplexNumber) ={
      eq.equiv(lft.a, rgt.a) && eq.equiv(lft.b, rgt.b)
    }
    return equiv
  }

  implicit class Equiv[A](lft: A) {
    def ====(rgt: A)(implicit eq: Eq[A]): Boolean = {
      eq.equiv(lft, rgt)
    }
  }

}

class ComplexNumber(val a:Double, val b:Double) {

  def ==( s: ComplexNumber):Boolean=
{
  a == s.a && b == s.b
}
  def +(s: ComplexNumber):ComplexNumber=
{
  new ComplexNumber(a+s.a,b+s.b)

}
  def *(s: ComplexNumber):ComplexNumber=
{
  new ComplexNumber(a*s.a-b*s.b,a*s.b+b*s.a)
}
  override def toString():String=
{
  s"$a+i$b"
}

  def ~(n: Int):ComplexNumber=
{
  var ans = new ComplexNumber(0,0)
  for(k <- 0 to n)
{
  val moodul =  (factorial(n)/(factorial(k)*factorial(n-k)))*Math.pow(a,n-k)*Math.pow(b,k)
  var p = -1
  if (k % 2 == 0 && k %4 == 0 || k % 2 == 1 && (k-1) %4 == 0) p = 1
  var item = new ComplexNumber(p*moodul,0)
  if(k%2 == 1){item = new ComplexNumber(0,p*moodul)}
  ans = ans+item

}
  ans
}

  def factorial(n: Int): Int = {
  if (n == 0)
  return 1
  else
  return n * factorial(n-1)
}
}
