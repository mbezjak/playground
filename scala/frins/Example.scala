import scala.language.postfixOps
import frins._

// https://github.com/martintrojer/frins
object run {
  initDatabases

  def sjR = 'sunradius / 'jupiterradius
  def sjM = 'sunmass / 'jupitermass
  def seR = 'sunradius / 'earthradius
  def seM = 'sunmass / 'earthmass

  Units.addUnit("sunvolume",   N(4/3, 'pi) * 'sunradius   ** 3)
  Units.addUnit("earthvolume", N(4/3, 'pi) * 'earthradius ** 3)
  def seV = 'sunvolume / 'earthvolume


  // http://van.physics.illinois.edu/qa/listing.php?id=25545
  def cellPhoneFreq  = N(1e9, 'Hz)
  def photonEnergy   = cellPhoneFreq * 'h
  def cellPhonePow   = N(1, 'W)
  def photonEmission = cellPhonePow / photonEnergy * 'photons
  // assuming isotropic emission
  def towerReception = photonEmission / (N(4, 'pi) * (N(1, 'km) ** 2))
}
