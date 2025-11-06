import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Success, Failure, Try}


object ChildBenefit { //have removed extends app and replaced with line 92 def main(args: Array[String]): Unit = {
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
    val eligible = children.filter(isChildEligible) //children.filter(isChildEligible) is used to create a list called eligible, which consists of children that meet certain eligibility criteria defined by the isChildEligible function.

    eligible match {
      case Nil => BigDecimal(0) // Case when there are no eligible children
      case _ if income <= 50000 => //Case 2 (uses -1 so if 3 children eldest child receives EldestChildRate, other 2 children receive FurtherChildRate so no double-counting the eldest child).
        BigDecimal(EldestChildRate) + (eligible.length - 1) * BigDecimal(FurtherChildRate)
      case _ if income >= 50001 && income <= 100000 && eligible.length == 1 => //Case 3
        BigDecimal(reducedRateOneChild)
      case _ if income >= 50001 && income <= 100000 && eligible.length >= 2 => //Case 4
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

  /** A Future represents a value that will be available at some point in the future & allows my program to continue running while waiting for potentially slow operations to complete. */

  /** Add "Future" (asynchronous) with OnComplete & Success/Failure
   Future is for async operations (things that take time) allowing non-blocking execution but may require more careful error handling if an exception occurs
   No pre-checking of conditions here: so if invalid data were passed, can lead to runtime exceptions during calculation**/
  //I just want to print something so will use Future[String]. Later can use Future[BigDecimal] if want to do more calculations.
  def calculateBenefitWithAsync(children: List[ChildInFamily], income: Int): Future[String] = { //Will return a string in the Future but not immediately, might need to use methods to extract the result.
    Future { //wrapped in Future-runs on separate thread-calculations processed asynchronously. Program doesn't get blocked/can continue executing other code.

      val weeklyAmount = finalTotalValue(children, income)
      val yearlyAmount = weeklyAmount * 52
      val eligibleCount = children.count(isChildEligible)
      val disabledCount = children.count(_.isDisabled)

      //Print string result (it returns then onComplete prints it)
      s"Eligible children: $eligibleCount, Disabled children: $disabledCount, Weekly benefit: £$weeklyAmount, Annual benefit: £$yearlyAmount"
    }
  }


  /** Add "Try" for synchronous error handling.
   It runs the code within the Try block immediately & blocks program until the calculation is complete. Result (success or failure) is given immediately
   Pre-checking here : In calculateBenefitWithTry, there are pre-checks for income and children's list, throwing specific exceptions if the conditions are not met. This checks the validity of inputs upfront and prevents unnecessary calculations**/
  def calculateBenefitWithTry(children: List[ChildInFamily], income: Int): Try[String] = {
    Try {
      // Try[String] can be success or failure, instead of crashing the program you can work with any error is synchronous manner.
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

    val youngFamilyBenefitCalculation = calculateBenefitWithAsync(youngFamily, youngFamilyTotalIncome) //calculateBenefitWithAsync starts the calculation & immediately returns a Future

    youngFamilyBenefitCalculation.onComplete { //onComplete registers a callback e.g. "when you finish, do this"
      case Success(result) => println("Family 1: two young children - " + result)
      case Failure(exception) => println(s"Processing your calculation failed: ${exception.getMessage}")
    }

    // Wait for Family 1 to complete before moving on / Await.ready blocks the main thread until the Future completes (or 5 seconds pass)
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
    println("Family 3: Using Try")
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


    /** .fold - handling success/failure in a single expression.
     Future doesn't actually have a .fold method in standard Scala!! There is Try.fold though not Future.fold.
     .map transforms the value inside a Future once it completes and returns a new Future with the transformed value **/

    // Family 4: Using.map

    println("Family 4: Using .map")
    val mapFamily = List(
      ChildInFamily(age = 8, inEducation = true, isDisabled = false),
      ChildInFamily(age = 5, inEducation = false, isDisabled = true),
      ChildInFamily(age = 2, inEducation = false, isDisabled = false)
    )
    val mapFamilyIncome = 48000

    val mapFamilyCalculation = calculateBenefitWithAsync(mapFamily, mapFamilyIncome) //calculateBenefitWithAsync returns Future[String]
      .map(result => result) //Future[String] of simple identity transformation here but could be diff e.g. map(result => result.toUpperCase)

    mapFamilyCalculation.onComplete {
      case Success(result) => println("Family 4: " + result)
      case Failure(exception) => println(s"Failed: ${exception.getMessage}")
    }

    try {
      Await.ready(mapFamilyCalculation, 5.seconds)
    } catch {
      case e: Exception => println(s"Family 4 had an issue but continuing...")
    }


    /** .recover - only handles failure! transforms failed Future into successful Future with a fallback value. So onComplete will almost always get a Success
     (If calculation succeeds → original result is returned, if calculation throws exception → recovery message is returned instead**/
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


      /** Family 6: Using .fold (handles both success and failure in one single expression) Instant decision (synchronous)!
       It immediately returns a value (not a Future, so no need for Await or onComplete)
       First parameter handles exceptions, second handles successful results. It is very synchronous (immediate result)!
       The .fold unwraps the Try container and gives actual value straight away**/
    println("Family 6: Using .fold with Try")
    val Family6UsingFold = List(
      ChildInFamily(age = 10, inEducation = true, isDisabled = false),
      ChildInFamily(age = 14, inEducation = true, isDisabled = true)
    )
    val Family6TotalIncome = 55000

    val Family6Result = calculateBenefitWithTry(Family6UsingFold, Family6TotalIncome) //calculateBenefitWithTry returns a Try[String], Try is a container that holds either Success(value) or Failure(exception) if something went wrong

    val Family6Calculation = Family6Result.fold(
      exception => s"Error occurred: ${exception.getMessage}", // Failure case
      result => result // Success case
    ) //Family6Output is now a plain string (not a Try anymore)

    println("Family 6: " + Family6Calculation)
//This example s good for simple error handling


      /** Family 7: Using .flatMap (handles nested Futures/nested types). Not Async, needs Await
       .flatMap is useful when need to chain multiple async operations & one Future depends on another and the need to avoid Future[Future[...]] nesting.
        Each operation depends on the previous ones result. Keeps code flat instead of deeply nested, without flatMap would have messy Futures within Futures.
      Using flatMap: transform value → returns Future[A] → result is Future[A] (flat Yay!)
      Without e.g.: transform value → returns Future[A] → result is Future[Future[A]] (nested Boo!**/
    println("Family 7: Using .flatMap for nested Futures")
    val Family7 = List(
      ChildInFamily(age = 4, inEducation = false, isDisabled = false),
      ChildInFamily(age = 7, inEducation = true, isDisabled = false)
    )
    val Family7Income = 42000

    // .flatMap flattens nested Futures: Future[Future[String]] becomes Future[String]
    val Family7Calculation = calculateBenefitWithAsync(Family7, Family7Income)  //calculateBenefitWithAsync returns Future[String]
      .flatMap { result => // Creates a nested Future, but flatMap flattens it automatically. There's another Future here...
        Future.successful(s"Here is your calculation: $result") // Returns Future[String]
      }

    Family7Calculation.onComplete {
      case Success(result) => println("Family 7: " + result)
      case Failure(exception) => println(s"Failed: ${exception.getMessage}")
    }

    try {
      Await.ready(Family7Calculation, 5.seconds)
    } catch {
      case e: Exception => println(s"Family 7 had an issue but system can continue...")
    }


//Next steps: Add Future.sequence: Calculate benefits for 5 families in parallel
    //Add .recoverWith: If calculation fails, retry with default values
    //Remove all Awaits: Use only callbacks (harder but more realistic)
    //Add timeout handling: What if a calculation takes too long?
    //Create a custom ExecutionContext: Use a fixed thread pool of 2 threads

    Thread.sleep(2000) //Let's all async prints finish
  }
}










