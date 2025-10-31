import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Success, Failure}


object ChildBenefit extends App {
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

  //  /** disabled child rate * */
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

  /** Add Future with OnComplete */
  //I just want to print something so will use Future[String. Later can use Future[BigDecimal] if want to do more calculations.
  def calculateBenefitWithAsync(children: List[ChildInFamily], income: Int): Future[String] = {
    Future {
      //Thread.sleep(1500) //This can be a fake delay just coding it here for learning purposes

      val weeklyAmount = finalTotalValue(children, income)
      val yearlyAmount = weeklyAmount * 52
      val eligibleCount = children.count(isChildEligible)
      val disabledCount = children.count(_.isDisabled)

      //Print string result (it returns then onComplete prints it)
      s"Eligible Children: $eligibleCount, Disabled Children: $disabledCount, Weekly Benefit: £$weeklyAmount, Annual Benefit: £$yearlyAmount"
    }
  }

    // Use it:

    val youngFamily = List(
      ChildInFamily(age = 3, inEducation = false, isDisabled = false),
      ChildInFamily(age = 1, inEducation = false, isDisabled = false)
    )

    val youngFamilyTotalIncome = 35000

    println("Processing your child benefit calculation") //This needs to print before the Future is created

    val youngFamilybenefitCalculation = calculateBenefitWithAsync(youngFamily, youngFamilyTotalIncome)


    youngFamilybenefitCalculation.onComplete {
      case Success(result) => println(result)
      case Failure(exception) => println(s"Processing your calculation failed: ${exception.getMessage}")
    }

    // Keep the program alive to get result. This is a Future outside of the Future above.
    Thread.sleep(2000)
  }


