import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Success, Failure, Try}


object ChildBenefit { //have removed extends app and replaced with line 92
  val EldestChildRate = 26.05 //per week
  val FurtherChildRate = 17.25 //per week
  val reducedRateOneChild = 2.88 //per week
  val reducedRateTwoOrMore = 5.77 //per week per child
  val additionalDisabledRateBenefit = 200.0 //per year

  def isChildEligible(childInFamily: ChildInFamily): Boolean = {
    if (childInFamily.age < 16) true
    else if (childInFamily.age < 20 && childInFamily.inEducation) true
    else false
  }

  def additionalDisabledBenefitRate(children: List[ChildInFamily], income: Int): BigDecimal = {
    val countChildrenWithDisability = children.filter(_.isDisabled == true)
    if (countChildrenWithDisability.nonEmpty && income <= 100000)
      countChildrenWithDisability.length * 3.85
    else
      BigDecimal(0)
  }

  def calculateWeeklyAmount(children: List[ChildInFamily], income: Int): BigDecimal = {
    val eligible = children.filter(isChildEligible)

    eligible match {
      case Nil => BigDecimal(0) // Case when there are no eligible children
      case _ if income <= 50000 =>
        BigDecimal(EldestChildRate) + (eligible.length - 1) * BigDecimal(FurtherChildRate)
      case _ if income >= 50001 && income <= 100000 && eligible.length == 1 =>
        BigDecimal(reducedRateOneChild)
      case _ if income >= 50001 && income <= 100000 && eligible.length >= 2 =>
        BigDecimal(reducedRateTwoOrMore) * eligible.length
      case _ => BigDecimal(0) // Default case
    }
  }

  def finalTotalValue(children: List[ChildInFamily], income: Int) = {
    calculateWeeklyAmount(children, income) + additionalDisabledBenefitRate(children, income)
  }

  /** disabled child rate * */
  def additionalDisabledBenefitRate(children: List[ChildInFamily]): Double = {
    children.count(_.isDisabled) * additionalDisabledRateBenefit
  }

  def calculateYearlyAmountEldest(): Double = {
    EldestChildRate * 52
  }

  def calculateYearlyAmountFurtherChild(): Double = {
    FurtherChildRate * 52
  }

  /** Add Future with OnComplete & Success/Failure
   * Future is for async operations (things that take time) */
  //I just want to print something so will use Future[String]. Later can use Future[BigDecimal] if want to do more calculations.
  def calculateBenefitWithAsync(children: List[ChildInFamily], income: Int): Future[String] = {
    Future {

      val weeklyAmount = finalTotalValue(children, income)
      val yearlyAmount = weeklyAmount * 52
      val eligibleCount = children.count(isChildEligible)
      val disabledCount = children.count(_.isDisabled)

      //Print string result (it returns then onComplete prints it)
      s"Eligible children: $eligibleCount, Disabled children: $disabledCount, Weekly benefit: £$weeklyAmount, Annual benefit: £$yearlyAmount"
    }
  }

  /** Add "Try" for synchronous error handling */
  def calculateBenefitWithTry(children: List[ChildInFamily], income: Int): Try[String] = {
    Try {
      // This could throw an exception if something goes wrong
      if (income < 0) throw new IllegalArgumentException("Income cannot be negative")
      if (children.isEmpty) throw new IllegalArgumentException("Must have at least one child")

      val weeklyAmount = finalTotalValue(children, income)
      val yearlyAmount = weeklyAmount * 52
      val eligibleCount = children.count(isChildEligible)
      val disabledCount = children.count(_.isDisabled)

      s"Eligible children: $eligibleCount, Disabled children: $disabledCount, Weekly benefit: £$weeklyAmount, Annual benefit: £$yearlyAmount"
    }
  }

  def main(args: Array[String]): Unit = {
    /** Using OnComplete & Success/Failure */
    //Family 1: have two young children
    val youngFamily = List(
      ChildInFamily(age = 3, inEducation = false, isDisabled = false),
      ChildInFamily(age = 1, inEducation = false, isDisabled = false)
    )

    val youngFamilyTotalIncome = 35000

    println("Processing your child benefit calculation...") //This needs to print before the Future is created

    val youngFamilyBenefitCalculation = calculateBenefitWithAsync(youngFamily, youngFamilyTotalIncome)


    youngFamilyBenefitCalculation.onComplete {
      case Success(result) => println("Family 1: two young children - " + result)
      case Failure(exception) => println(s"Processing your calculation failed: ${exception.getMessage}")
    }

    // Wait for Family 1 to complete before moving on
    Await.ready(youngFamilyBenefitCalculation, 5.seconds)


    //Family 2: Single child family
    val singleChildFamily = List(
      ChildInFamily(age = 6, inEducation = true, isDisabled = false)
    )
    val singleChildIncome = 65000 // The child benefit reduced rate is applied

    println("Family 2: Single child & higher earnings")
    val singleChildFamilybenefitCalculation = calculateBenefitWithAsync(singleChildFamily, singleChildIncome)
    singleChildFamilybenefitCalculation.onComplete {
      case Success(result) => println(result + "\n")
      case Failure(exception) => println(s"Failed: ${exception.getMessage}\n")
    }

    Await.ready(singleChildFamilybenefitCalculation, 5.seconds)
    Thread.sleep(100)

    /** Try is good for synchronous error handling */
    // Family 3: Using Try for synchronous validation
    println("\nFamily 3: Using Try")
    val tryFamily = List(
      ChildInFamily(age = 12, inEducation = true, isDisabled = true),
      ChildInFamily(age = 19, inEducation = false, isDisabled = false)
    )
    val tryFamilyIncome = 45000

    val tryResult = calculateBenefitWithTry(tryFamily, tryFamilyIncome)
    tryResult match {
      case Success(result) => println(result + "\n")
      case Failure(exception) => println(s"Validation failed: ${exception.getMessage}")
    }

    Thread.sleep(1000)


    /** .fold - handling success/failure in a single expression */
    // Family 4: Using.map (transforms successful Future results)

    println("Family 4: Using .map")
    val mapFamily = List(
      ChildInFamily(age = 8, inEducation = true, isDisabled = false),
      ChildInFamily(age = 5, inEducation = false, isDisabled = true),
      ChildInFamily(age = 2, inEducation = false, isDisabled = false)
    )
    val mapFamilyIncome = 48000

    val mapFamilyCalculation = calculateBenefitWithAsync(mapFamily, mapFamilyIncome)
      .map(result => result)

    mapFamilyCalculation.onComplete {
      case Success(result) => println("Family 4: " + result)
      case Failure(exception) => println(s"Failed: ${exception.getMessage}")
    }

    try {
      Await.ready(mapFamilyCalculation, 5.seconds)
    } catch {
      case e: Exception => println(s"Family 4 had an issue but continuing...")
    }


    /** .recover - transforms a failed Future into a successful Future with a fallback value. So onComplete will almost always get a Success*/
    //Family 5: Using .recover
    println("Family 5: Using .recover")
    val Family5UsingRecover = List(
      ChildInFamily(age = 15, inEducation = true, isDisabled = false),
      ChildInFamily(age = 17, inEducation = true, isDisabled = false)
    )
    val Family5TotalIncome = 78000

    val Family5Calculation = calculateBenefitWithAsync(Family5UsingRecover, Family5TotalIncome)
      .recover {
        case e: Exception => s"I have recovered from error: ${e.getMessage} - now using a default calculation"
      }

    Family5Calculation.onComplete {
      case Success(result) => println("Family 5:" + result)
      case Failure(exception) => println(s"Failed: ${exception.getMessage}")
    }

    try {
      Await.ready(Family5Calculation, 5.seconds)
    } catch {
      case e: Exception => println(s"The calculation for Family 5 had an issue but system continues...")
    }


    Thread.sleep(2000) //Let's all async prints finish
  }
}








