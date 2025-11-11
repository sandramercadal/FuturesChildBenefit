import scala.concurrent.{Future, Await, Promise}
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

  /** A Future represents a value that will be available at some point in the future & allows my program to continue running while waiting for potentially slow operations to complete. One advantage of Scala's futures is that they help you avoid blocking, you can keep the finite number of threads you decide to work with busy*/


  /** Add "Future" with OnComplete & Success/Failure(asynchronous)
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
    /** Family 1 & 2: Using OnComplete & Success/Failure
     I want to do work in the background and handle the result when it's ready**/
    //Family 1: have two young children
    val youngFamily = List(
      ChildInFamily(age = 3, inEducation = false, isDisabled = false), //from the case class called ChildInFamily
      ChildInFamily(age = 1, inEducation = false, isDisabled = false)
    )

    val youngFamilyTotalIncome = 35000

    println("Processing your child benefit calculation...") //This needs to print before the Future is created

    val youngFamilyBenefitCalculation = calculateBenefitWithAsync(youngFamily, youngFamilyTotalIncome) //calculateBenefitWithAsync starts the calculation & immediately returns a Future

    youngFamilyBenefitCalculation.onComplete { //onComplete registers a callback e.g. "when you finish, do this"
      case Success(result) => println("Family 1: two young children - " + result)
      case Failure(exception) => println(s"Processing your calculation for Family 1 failed: ${exception.getMessage}")
    }

    // Wait for Family 1 to complete before moving on / Await.ready blocks the main thread until the Future completes (or 5 seconds pass)
    Await.ready(youngFamilyBenefitCalculation, 5.seconds)


    //Family 2: Single child family with Await.ready
    /** Await.ready just waits for completion, it doesn't extract the value & returns the Future itself */
    val singleChildFamily = List(
      ChildInFamily(age = 6, inEducation = true, isDisabled = false)
    )
    val singleChildIncome = 65000 // The child benefit reduced rate is applied

    println("Family 2: Single child & higher earnings")
    val singleChildFamilybenefitCalculation = calculateBenefitWithAsync(singleChildFamily, singleChildIncome)
    singleChildFamilybenefitCalculation.onComplete {
      case Success(result) => println(result + "\n")
      case Failure(exception) => println(s"Family 2 failure: ${exception.getMessage}\n")
    }

    Await.ready(singleChildFamilybenefitCalculation, 5.seconds)
    Thread.sleep(100)


    /** Await.ready vs Await.result
     Await.result blocks. Do you definitely need the result immediately? (therefore blocking/synchronous).
     It waits for completion, it extracts the actual value and returns the value directly (not the Future)**/
    //Family 2a: An example with Await.ready with 2 threads running
    println("Family 2a: Using Await.result to get answer immediately")

    val family2aAwaitResult = List(
      ChildInFamily(age = 6, inEducation = true, isDisabled = false),
      ChildInFamily(age = 2, inEducation = false, isDisabled = true),
      ChildInFamily(age = 11, inEducation = true, isDisabled = false)
    )
    val family2aIncome = 44400

    // Start async calc, starts running on a separate thread immediately, does calculation work in background of weekly & yearly amount etc
    val family2aCalculation = calculateBenefitWithAsync(family2aAwaitResult, family2aIncome) //calculateBenefitWithAsync returns a Future[String]
   //The Future { } wrapper inside the function is what actually starts the helper. family2aCalculation now holds the "result"

    println("Family 2a calculation begun, now blocking until I get the result...")

    try {
      // Await.result blocks here and returns the actual string value
      val benefitResult: String = Await.result(family2aCalculation, 6.seconds)

      // Now we have the actual value, this is not a Future
      println(s"Family 2a with Await.ready: Got immediate result - $benefitResult")

    } catch {
      case _: scala.concurrent.TimeoutException =>
        println("Family 2a Await.ready: Calculation took too long oops!")
      case e: Exception =>
        println(s"Family 2a Await.ready: Calculation failed - ${e.getMessage}")
    }


    /** Family 3: Using Try for synchronous validation, Try is good for synchronous error handling
     Want result immediately (not async) **/
    println("Family 3: Using Try")
    val tryFamily = List(
      ChildInFamily(age = 12, inEducation = true, isDisabled = true),
      ChildInFamily(age = 19, inEducation = false, isDisabled = false)
    )
    val tryFamilyIncome = 45000

    val tryResult = calculateBenefitWithTry(tryFamily, tryFamilyIncome)
    tryResult match {
      case Success(result) => println(result + "\n")
      case Failure(exception) => println(s"Family 3 validation failed: ${exception.getMessage}")
    }

    Thread.sleep(1000)


    /** Family 4: .fold - handling success/failure in a single expression.
     Future doesn't actually have a .fold method in standard Scala!! There is Try.fold though not Future.fold.
     .map transforms the value inside a Future once it completes and returns a new Future with the transformed value **/

    //Using.map
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
      case Failure(exception) => println(s"Family 4 failed: ${exception.getMessage}")
    }

    try {
      Await.ready(mapFamilyCalculation, 5.seconds)
    } catch {
      case e: Exception => println(s"Family 4 had an issue but continuing...")
    }


    /** Family 5: Using .recover - only handles failure! transforms failed Future into successful Future with a fallback value. So onComplete will almost always get a Success
     (If calculation succeeds → original result is returned, if calculation throws exception → recovery message is returned instead**/
    println("Family 5: Using .recover to handle failures")
    val Family5UsingRecover = List(
      ChildInFamily(age = 15, inEducation = true, isDisabled = false),
      ChildInFamily(age = 17, inEducation = true, isDisabled = false)
    )
    val Family5TotalIncome = 78000

    val Family5Calculation = calculateBenefitWithAsync(Family5UsingRecover, Family5TotalIncome)
      .recover {
        case e: Exception => s"Family 5 - I have recovered from error: ${e.getMessage} - now using a default calculation"
      }

    Family5Calculation.onComplete {
      case Success(result) => println("Family 5:" + result)
      case Failure(exception) => println(s"Family 5 failed: ${exception.getMessage}")
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
      exception => s"Error occurred for Family 6: ${exception.getMessage}", // Failure case
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
        Future.successful(s"Here is your calculation for Family 7: $result") // Returns Future[String]
      }

    Family7Calculation.onComplete {
      case Success(result) => println("Family 7: " + result)
      case Failure(exception) => println(s"Family 7 failed: ${exception.getMessage}")
    }

    try {
      Await.ready(Family7Calculation, 5.seconds)
    } catch {
      case e: Exception => println(s"Family 7 had an issue but system can continue...")
    }


    /** Family 8 - Processes 4 families simultaneously using Future.sequence so all calculations which are independent of each other run in parallel (instead of one after another - efficient!)**/
    println("Family 8: Using Future.sequence for parallel processing")

    val families8ForParallelProcessing = List(
      (List(ChildInFamily(age = 4, inEducation = false, isDisabled = false)), 38000, "Calculation for Family 8 -The Smiths"),
      (List(ChildInFamily(age = 10, inEducation = true, isDisabled = true), ChildInFamily(age = 7, inEducation = true, isDisabled = false)), 51000, "Calculation for Family 8 -The Browns"),
      (List(ChildInFamily(age = 16, inEducation = true, isDisabled = false)), 67500, "Calculation for Family 8 -The Martins"),
      (List(ChildInFamily(age = 3, inEducation = false, isDisabled = false), ChildInFamily(age = 5, inEducation = true, isDisabled = false), ChildInFamily(age = 8, inEducation = true, isDisabled = false)), 45000, "Calculation for Family 8 -The Jones")
    )

    // List of Futures which will all start running in parallel immediately
    val family8ParallelCalculations: List[Future[String]] = families8ForParallelProcessing.map {
      case (children, income, familyNameFromFamily8) =>
        calculateBenefitWithAsync(children, income).map(result => s"$familyNameFromFamily8: $result")
    }

    // Future.sequence converts List[Future[String]] into Future[List[String]] e.g Future(["result1", "result2", "result3", "result4"])
    val allFamilyResults: Future[List[String]] = Future.sequence(family8ParallelCalculations)

    allFamilyResults.onComplete {
      case Success(results) =>
        println("All parallel calculations are now complete for all of Family 8:")
        results.foreach(println)
      case Failure(exception) => println(s"Family 8 failure - parallel calculations failed: ${exception.getMessage}")
    }

    try {
      Await.ready(allFamilyResults, 10.seconds)
    } catch {
      case e: Exception => println(s"Family 8 parallel processing had an issue but system continues...")
    }


    /** Family 9 - for-comprehension with Futures (Syntactic sugar for .flatMap and .map so chained Futures are more readable)
     sequential async operations/chaining
     multiple Futures that depend on each other **/
    println("Family 10: Using for-comprehension")

    val family9a = List(
      ChildInFamily(age = 6, inEducation = true, isDisabled = false)
    )
    val familyIncome9a = 39250

    val family9b = List(
      ChildInFamily(age = 13, inEducation = true, isDisabled = true),
      ChildInFamily(age = 15, inEducation = true, isDisabled = false)
    )
    val familyIncome9b = 58000

    val family9c = List(
      ChildInFamily(age = 7, inEducation = true, isDisabled = false),
      ChildInFamily(age = 10, inEducation = true, isDisabled = false)
    )
    val familyIncome9c = 44500

    val Family9Calculation: Future[String] = for {
      family9aTotal <- calculateBenefitWithAsync(family9a, familyIncome9a) //uses def calculateBenefitWithAsync(children: List[ChildInFamily], income: Int): Future[String]
      family9bTotal <- calculateBenefitWithAsync(family9b, familyIncome9b)
      family9cTotal <- calculateBenefitWithAsync(family9c, familyIncome9c)
    } yield s"Family 9a: $family9aTotal, Family 9b: $family9bTotal, Family 9c: $family9cTotal"

    Family9Calculation.onComplete {
      case Success(result) => println(result)
      case Failure(exception) => println(s"Family 9 failed: ${exception.getMessage}")
    }

    try {
      Await.ready(Family9Calculation, 3.seconds)
    } catch {
      case e: Exception => println(s"Family 9 had an issue but system continues...")
    }


      /** Family 10 - Using Promise. Use when need to complete a Future based on external events, callbacks, or conditions that are unpredictable or outside your control.
      so e.g. "I have the answer now but need to wait for XYZ (you manually control when & how a Future completes)
       Promise is a writable, single-assignment container that completes a Future.
       Future completes when Promise is fulfilled**/
    println("Family 10: Using Promise. Manager approval of benefit with a Promise")

    val family10WithPromise = List(
      ChildInFamily(age = 4, inEducation = false, isDisabled = false),
      ChildInFamily(age = 9, inEducation = true, isDisabled = true)
    )
    val familyIncome10 = 87000

    val family10Promise = Promise[String]() //Create an empty promise that is waiting to be filled. Import promise so need to type scala.concurrent.Promise[String]()
    val getFamily10PromiseFromFuture: Future[String] = family10Promise.future //Get the promise

    Future { //runs on a separate thread. Manager approval process is happening in background
      Thread.sleep(500)
      val weeklyAmount = finalTotalValue(family10WithPromise, familyIncome10) //First do the actual calculations
      val yearlyAmount = weeklyAmount * 52
      val eligibleCount = family10WithPromise.count(isChildEligible)
      val disabledCount = family10WithPromise.count(_.isDisabled)

      family10Promise.success(  //fill the promise with the result
        s"Family 10 eligible children: $eligibleCount, Family 10 disabled children: $disabledCount, Family 10 weekly benefit: £$weeklyAmount, Family 10 annual benefit: £$yearlyAmount"
      )
    }

    getFamily10PromiseFromFuture.onComplete { //call back, when Futures completes do below
      case Success(result) => println("Family 10: " + result)
      case Failure(exception) => println(s"Family 10 failed: ${exception.getMessage}")
    }

    try { //wait here until Future finishes (max 5 seconds) which blocks main program so it doesn't exit before family 10 finishes
      Await.ready(getFamily10PromiseFromFuture, 5.seconds)
    } catch {
      case e: Exception => println(s"Family 10 had an issue but system continues...")
    }


    /** Family 11 - Uses .zip to combine two independent Futures into a single tuple. Runs in parallel, zip waits for both Futures to complete, then combines their results into a tuple useful when you need results from two independent async operations, you need to compare or combine the two results and both can run at the same time **/
    println("Family 11: Using .zip to combine two calculations")

    val family11a = List(
      ChildInFamily(age = 7, inEducation = true, isDisabled = false),
      ChildInFamily(age = 10, inEducation = true, isDisabled = true)
    )
    val familyIncome11a = 39500

    val family11b = List(
      ChildInFamily(age = 4, inEducation = false, isDisabled = false),
      ChildInFamily(age = 6, inEducation = true, isDisabled = false),
      ChildInFamily(age = 11, inEducation = true, isDisabled = false)
    )
    val familyIncome11b = 54000

    // Start both calculations at the same time (in parallel)
    val calculation11a = calculateBenefitWithAsync(family11a, familyIncome11a)
    val calculation11b = calculateBenefitWithAsync(family11b, familyIncome11b)

    println("Family 11: Both calculations started in parallel...")

    // zip combines both results into a tuple: (resultA, resultB)
    val combinedCalculation = calculation11a.zip(calculation11b)

    combinedCalculation.onComplete {
      case Success((resultA, resultB)) =>
        println("Family 11: Both calculations ran in parallel and are complete!")
        println(s"  Family 11A: $resultA")
        println(s"  Family 11B: $resultB")

        // Can now compare or combine the two results
        println(s"  Processing complete for both families in Family 11")

      case Failure(exception) =>
        println(s"Family 11: At least one calculation failed - ${exception.getMessage}")
    }

    try {
      Await.ready(combinedCalculation, 6.seconds)
    } catch {
      case e: Exception => println(s"Family 11 had an issue but system continues...")
    }


    /** Family 12 - Customised thread pools - Create a custom ExecutionContext: Use fixed thread pool of 3 threads when 5 families arrive at once.
     E.g only 3 checkout till lanes at a supermarket open, if 5 customers arrive at once, 2 will have to wait. My computer decides how many helpers to create (usually 1 helper per CPU core), If 8 CPU cores = 8 helpers available
      Lots of helpers = lots of work happens at once

     Good to understand that only a limited number of Futures truly run in parallel.*/

    println("Family 12: 5 families, but only 3 threads - so some families must wait like in a supermarket when there are not enough checkouts open")

    val family12A = (List(
      ChildInFamily(age = 4, inEducation = false, isDisabled = false),
      ChildInFamily(age = 10, inEducation = true, isDisabled = false)
    ), 38000)

    val family12B = (List(
      ChildInFamily(age = 10, inEducation = true, isDisabled = true)
    ), 42500)

    val family12C = (List(
      ChildInFamily(age = 12, inEducation = true, isDisabled = false),
      ChildInFamily(age = 16, inEducation = true, isDisabled = false),
      ChildInFamily(age = 5, inEducation = true, isDisabled = false)
    ), 56000)

    val family12D = (List(
      ChildInFamily(age = 3, inEducation = false, isDisabled = false)
    ), 68000)

    val family12E = (List(
      ChildInFamily(age = 15, inEducation = true, isDisabled = false),
      ChildInFamily(age = 17, inEducation = true, isDisabled = true)
    ), 97000)

    // Start all 6 calculations at once/call calculateBenefitWithAsync 5 times
    val calcFamily2A = calculateBenefitWithAsync(family12A._1, family12A._2)  // 1= children list, 2= income
    val calcFamily2B = calculateBenefitWithAsync(family12B._1, family12B._2)
    val calcFamily2C = calculateBenefitWithAsync(family12C._1, family12C._2)
    val calcFamily2D = calculateBenefitWithAsync(family12D._1, family12D._2)
    val calcFamily2E = calculateBenefitWithAsync(family12E._1, family12E._2)

    // Combine all 5 calls into a list with Future.sequence
    val allFamily12Results = Future.sequence(List(calcFamily2A, calcFamily2B, calcFamily2C, calcFamily2D, calcFamily2E))

    allFamily12Results.onComplete {
      case Success(results) =>
        println("Family 12 - All 5 calculations are complete on 3 threads: first 3 ran and then the next 2 ran")
        results.foreach(println)
      case Failure(exception) =>
        println(s"Family 12 failed: ${exception.getMessage}")
    }
    Await.ready(allFamily12Results, 10.seconds)


    /**Family 13 - Some Futures don't require an execution context!
     With future.successful no Execution Context needed. YOu already have the answer but need to return a Future.
     The work is already done for you right now on the main thread before you start the Future. No background threads.
     Just wrap an existing value*/

    val family13 = List(
      ChildInFamily(age = 14, inEducation = true, isDisabled = false),
      ChildInFamily(age = 16, inEducation = true, isDisabled = false)
    )
    val familyIncome13 = 39000

    // Do calculation right now (synchronously) on current thread. Happens immediately, no background threads. no helper/worked needed.
    val weeklyAmount = finalTotalValue(family13, familyIncome13)
    val yearlyAmount = weeklyAmount * 52
    val eligibleCount = family13.count(isChildEligible)

    // Wrap the result which already exists in an already-completed Future.
    val family13ImmediateResult: Future[String] = Future.successful(
      s"Family 13 Eligible children: $eligibleCount, Family 13 Weekly benefit: £$weeklyAmount, Family 13 Annual benefit: £$yearlyAmount"
    )
    // This Future is ALREADY complete when created - no waiting needed!

    family13ImmediateResult.onComplete {
      case Success(result) => println(s"Family 13 with no Execution Context needed: $result")
      case Failure(exception) => println(s"Family 13 failed: ${exception.getMessage}")
    }


    Thread.sleep(2000) //Let's all async prints finish
  }
}

/** S U M M A R Y
 -------------------

 Future = A task happening somewhere else
 Futures let you write responsive programs that don't freeze while waiting. Instead of standing in one queue, you can send "helpers" to multiple queues at once and collect results when ready = foundation of modern async programming in Scala!

 You get a "ticket" immediately - the work happens in the background - eventually the ticket "completes" with a result - can chain operations on the ticket before it completes

Best not to:
 -------------
 block async code (use .map/.flatMap),
mix sync and sync,
have nested Futures (use .flatMap)
Not handle the Future (use .recover or error handling).

-----------------------------------
** M Y   F  A  M  I  L  I  E  S **
-----------------------------------
CODE         / WHEN TO USE          / FAMILY:

 onComplete / Handle async result  / Family 1, 2
 Await.result / Need value now (blocks) / Family 2a
 Try / Sync error handling / Family 3
 .map / Transform result / Family 4
 .recover / Fallback on failure / Family 5
 .fold / Handle both cases instantly / Family 6 (there is no actual .fold use .map)
 .flatMap / Chain dependent Futures / Family 7
 Future.sequence / Many Futures → One result / Family 8
 for-comprehension / Sequential chaining (clean) / Family 9
 Promise / Manual completion / Family 10
 .zip / Combine 2 Futures / Family 11
 Customised thread pools / Control concurrency / Family 12
 Future.successful for when no execution Context / Already have a result as no execution Context/ Family 13 **/












//Next steps possibly:
//Add .recoverWith: If calculation fails, retry with default values?
//Remove all Awaits: Use only callbacks (hard?)
//Add timeout handling: What if a calculation takes too long?

//foldLeft
//2) flatten, zipWith, transform with
//how to test a future???

//harder??
//validate a future with a filter and collect?





