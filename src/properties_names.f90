module properties_names
    use constants

    type PropertiesNames
        character(len=:), allocatable :: s
    end type PropertiesNames

contains

    function arrays_of_properties_names_ChemSep() result(names)
        implicit none
        type (PropertiesNames) :: names(properties_number)

        names(1)%s = 'LibraryIndex'
        names(2)%s = 'CompoundID'
        names(3)%s = 'StructureFormula'
        names(4)%s = 'Family'
        names(5)%s = 'CriticalTemperature'
        names(6)%s = 'CriticalPressure'
        names(7)%s = 'CriticalVolume'
        names(8)%s = 'CriticalCompressibility'
        names(9)%s = 'NormalBoilingPointTemperature'
        names(10)%s = 'NormalMeltingPointTemperature'
        names(11)%s = 'TriplePointTemperature'
        names(12)%s = 'TriplePointPressure'
        names(13)%s = 'MolecularWeight'
        names(14)%s = 'LiquidVolumeAtNormalBoilingPoint'
        names(15)%s = 'AcentricityFactor'
        names(16)%s = 'RadiusOfGyration'
        names(17)%s = 'SolubilityParameter'
        names(18)%s = 'DipoleMoment'
        names(19)%s = 'VanDerWaalsVolume'
        names(20)%s = 'VanDerWaalsArea'
        names(21)%s = 'HeatOfFormation'
        names(22)%s = 'GibbsEnergyOfFormation'
        names(23)%s = 'AbsEntropy'
        names(24)%s = 'HeatOfFusionAtMeltingPoint'
        names(25)%s = 'HeatOfCombustion'
        names(26)%s = 'SolidDensity'
        names(27)%s = 'LiquidDensity'
        names(28)%s = 'VaporPressure'
        names(29)%s = 'HeatOfVaporization'
        names(30)%s = 'SolidHeatCapacityCp'
        names(31)%s = 'LiquidHeatCapacityCp'
        names(32)%s = 'IdealGasHeatCapacityCp'
        names(33)%s = 'SecondVirialCoefficient'
        names(34)%s = 'LiquidViscosity'
        names(35)%s = 'VaporViscosity'
        names(36)%s = 'LiquidThermalConductivity'
        names(37)%s = 'VaporThermalConductivity'
        names(38)%s = 'SurfaceTension'
        names(39)%s = 'RPPHeatCapacityCp'
        names(40)%s = 'AntoineVaporPressure'
        names(41)%s = 'LiquidViscosityRPS'
        names(42)%s = 'COSTALDVolume'
        names(43)%s = 'DiameterLJ'
        names(44)%s = 'EnergyLJ'
        names(45)%s = 'RacketParameter'
        names(46)%s = 'FullerVolume'
        names(47)%s = 'Parachor'
        names(48)%s = 'SpecificGravity'
        names(49)%s = 'Charge'
        names(50)%s = 'CostaldAcentricFactor'
        names(51)%s = 'WilsonVolume'
        names(52)%s = 'UniquacR'
        names(53)%s = 'UniquacQ'
        names(54)%s = 'ChaoSeaderAcentricFactor'
        names(55)%s = 'ChaoSeaderSolubilityParameter'
        names(56)%s = 'ChaoSeaderLiquidVolume'
        names(57)%s = 'UnifacVLE'
        names(58)%s = 'Umr'
        names(59)%s = 'CAS'
        names(60)%s = 'Smiles'
        


    end function arrays_of_properties_names_ChemSep

end module properties_names