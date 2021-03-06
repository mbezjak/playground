import math._

val AU  = 1.49597870691e11  // m
val pc  = 206265 * AU       // m - parsec
val ly  = 9.460536207e15    // m - light year
val yr  = 365.2564          // days - sidereal year
val y2s = yr * 24 * 60 * 60 // s - seconds in a year
val ec  = 1.6021765e-19     // C - elementary charge
val tnt = 4.2e6             // J - explosion of a kg of TNT
val amu = 1.66053886e-27    // kg - atomic mass unit

val c  = 2.99792458e8     // m/s
val G  = 6.673e-11        // m^3 kg^-1 s^-2
val me = 9.109382e-31     // kg - electron mass
val mp = 1.6726217e-27    // kg - proton mass
val mn = 1.6749274e-27    // kg - neutron mass
val ma = 6.64465675e-27   // kg - alpha particle mass
val b  = 0.0028977685     // m K - wien's constant
val kb = 1.380648e-23     // J/K - boltzmann constant
val sigma = 5.6703e-8     // W m^-2 K^-4 - stafan-boltzmann constant
val h  = 6.626069e-34     // J s - planck constant

// astronomical
val Msun = 1.9891e30    // kg
val Rsun = 6.95508e8    // m
val Lsun = 3.83e26      // W
val bsun = 1370         // W m^-2 - solar constant
val Tsun = 5777         // K
val Mearth = 5.972e24   // kg
val Rearth = 6.371e6    // m
val Tearth = 288        // K
val Learth = 4 * Pi * pow(Rearth, 2) * sigma * pow(Tearth, 4) // W
