package web.sites

import java.util.concurrent.TimeUnit

import scala.collection.JavaConverters._
import org.openqa.selenium.{By, JavascriptExecutor, WebElement}
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.support.ui.Select
import web.{conf, getDriver}

import scala.App
import scala.collection.mutable


object FinCEN extends App {

  val driver = getDriver(20)
  driver.get("https://bsaefiling1.fincen.treas.gov/lc/content/xfaforms/profiles/htmldefault.html")

  Thread.sleep(500)

  val eltEmail = driver.findElement(By.name("Email_5"))
  eltEmail.sendKeys(conf().getString("email"))

  Thread.sleep(500)

  val eltConfirmEmail = driver.findElement(By.name("ConfirmedEmail_6"))
  eltConfirmEmail.sendKeys(conf().getString("email"))

  Thread.sleep(500)

  val eltFirstName = driver.findElement(By.name("FirstName_7"))
  eltFirstName.sendKeys(conf().getString("firstName"))

  Thread.sleep(500)

  val eltLastName = driver.findElement(By.name("LastName_8"))
  eltLastName.sendKeys(conf().getString("lastName"))

  Thread.sleep(500)

  val eltFilingName = driver.findElement(By.name("FilingName_23"))
  eltFilingName.sendKeys(conf().getString("filingName"))

  Thread.sleep(500)

  val eltFilerType = driver.findElement(By.name("FilerType_61"))
  new Select(eltFilerType).selectByValue("A")

  Thread.sleep(500)

  val eltFilerTIN = driver.findElement(By.name("TIN_64"))
  eltFilerTIN.sendKeys(conf().getString("SSN"))

  Thread.sleep(500)

  val eltFilerTINType = driver.findElement(By.name("TINTYPE_65"))
  new Select(eltFilerTINType).selectByValue("B")

  Thread.sleep(500)

  val eltFilerBirthMonth = driver.findElement(By.name("month_79"))
  new Select(eltFilerBirthMonth).selectByValue(conf().getString("birthMonth"))

  Thread.sleep(500)

  val eltFilerBirthDay = driver.findElement(By.name("day_80"))
  new Select(eltFilerBirthDay).selectByValue(conf().getString("birthDay"))

  Thread.sleep(500)

  val eltFilerBirthYear = driver.findElement(By.name("year_78"))
  new Select(eltFilerBirthYear).selectByValue(conf().getString("birthYear"))

  Thread.sleep(500)

  val eltLastNameAgain = driver.findElement(By.name("LastName_77"))
  eltLastNameAgain.sendKeys(conf().getString("lastName"))

  val eltFirstNameAgain = driver.findElement(By.name("FirstName_82"))
  eltFirstNameAgain.sendKeys(conf().getString("firstName"))

  val eltMiddleName = driver.findElement(By.name("MiddleName_83"))
  eltMiddleName.sendKeys(conf().getString("middleName"))

  Thread.sleep(500)

  val eltCountryIndividual = driver.findElement(By.name("CountryIndividual_89"))
  new Select(eltCountryIndividual).selectByValue("BR ")

  Thread.sleep(1000)

  val eltZIP = driver.findElement(By.name("ZIP_88"))
  eltZIP.sendKeys(conf().getString("zip"))

  val eltStreetAddress = driver.findElement(By.name("Address_85"))
  eltStreetAddress.sendKeys(conf().getString(("streetAddress")))

  val eltCity = driver.findElement(By.name("City_86"))
  eltCity.sendKeys(conf().getString(("city_en")))

  val eltNoMoreThan25 = driver.findElement(By.name("InterestIn25OrMoreNo_93"))
  if (! eltNoMoreThan25.isSelected) eltNoMoreThan25.click()

  val eltNoSigAuthMoreThan25 = driver.findElement(By.name("SigAuthIn25OrMoreNo_100"))
  if (! eltNoSigAuthMoreThan25.isSelected) eltNoSigAuthMoreThan25.click()

  Thread.sleep(500)

  val accounts = conf().getConfigList("accounts").asScala
  case class Account(value:String, nnn:String, finame:String, address:String, city:String, country:String, zzz:String)
  val eltAddPt2 = driver.findElement(By.name("Addpart2_107"))
  for (i <- 2 to accounts.length) {
    eltAddPt2.click()
    Thread.sleep(1000)
  }

  driver.findElement(By.name("MaxAcctValue_109")).sendKeys(accounts(0).getString("value"))
  driver.findElement(By.name("FIAcctName_116")).sendKeys(accounts(0).getString("finame"))
  driver.findElement(By.name("AcctNumber_118")).sendKeys(accounts(0).getString("nnn"))
  driver.findElement(By.name("Address_119")).sendKeys(accounts(0).getString("address"))
  driver.findElement(By.name("City_120")).sendKeys(accounts(0).getString("city"))
  driver.findElement(By.name("ZIP_123")).sendKeys(accounts(0).getString("zzz"))
  new Select(driver.findElement(By.name("Country_121"))).selectByValue(accounts(0).getString("country") ++ " ")
  new Select(driver.findElement(By.name("AccountType_111"))).selectByValue(accounts(0).getString("type"))

  val allInputs = driver.findElementsByTagName("input").asScala
  val allSelects = driver.findElementsByTagName("select").asScala
  val allTxtAreas = driver.findElementsByTagName("textarea").asScala

  val maxVals = allInputs.filter(_.getAttribute("name") contains("MaxAcctValueCL")).toList
  val acctTypes = allSelects.filter(_.getAttribute("name") contains("AccountTypeCL")).toList
  val accNs = allInputs.filter(_.getAttribute("name") contains("AcctNumberCL")).toList
  val accFINames = allTxtAreas.filter(_.getAttribute("name") contains("FIAcctNameCL")).toList
  val accAddresses = allTxtAreas.filter(_.getAttribute("name") contains("AddressCL")).toList
  val accCities = allTxtAreas.filter(_.getAttribute("name") contains("CityCL")).toList
  val accCountries = allSelects.filter(_.getAttribute("name") contains("CountryCL")).toList
  val accZIPs = allInputs.filter(_.getAttribute("name") contains("ZIPCL")).toList

  println(maxVals.length.toString)
  println(acctTypes.length.toString)
  println(accNs.length.toString)
  println(accFINames.length.toString)
  println(accAddresses.length.toString)
  println(accCities.length.toString)
  println(accCountries.length.toString)
  println(accZIPs.length.toString)

  trait X
  case class Bank() extends X

  println(s"accounts length is ${accounts.length.toString}")
  for (i <- 1 to (accounts.length - 1) ) {
    maxVals(i - 1).sendKeys(accounts(i).getString("value"))
    accNs(i - 1).sendKeys(accounts(i).getString("nnn"))
    accFINames(i - 1).sendKeys(accounts(i).getString("finame"))
    accAddresses(i - 1).sendKeys(accounts(i).getString("address"))
    accCities(i - 1).sendKeys(accounts(i).getString("city"))
    accZIPs(i - 1).sendKeys(accounts(i).getString("zzz"))
    new Select(accCountries(i-1)).selectByValue(accounts(i).getString("country") ++ " ")
    new Select(acctTypes(i-1)).selectByValue(accounts(i).getString("type"))
  }


}
