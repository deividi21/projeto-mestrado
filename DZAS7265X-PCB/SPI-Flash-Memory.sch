EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 3 4
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L AT25SF041B-SHD-B:AT25SF041B-SHD-B U?
U 1 1 61530862
P 2375 2225
F 0 "U?" H 2375 2892 50  0000 C CNN
F 1 "AT25SF041B-SHD-B" H 2375 2801 50  0000 C CNN
F 2 "SOIC127P790X216-8N" H 2375 2225 50  0001 L BNN
F 3 "" H 2375 2225 50  0001 L BNN
F 4 "IPC 7351B" H 2375 2225 50  0001 L BNN "STANDARD"
F 5 "F" H 2375 2225 50  0001 L BNN "PARTREV"
F 6 "Dialog Semicondutor" H 2375 2225 50  0001 L BNN "MANUFACTURER"
F 7 "2.16mm" H 2375 2225 50  0001 L BNN "MAXIMUM_PACKAGE_HEIGHT"
	1    2375 2225
	1    0    0    -1  
$EndComp
$Comp
L power:+3.3V #PWR?
U 1 1 61530868
P 3150 1350
F 0 "#PWR?" H 3150 1200 50  0001 C CNN
F 1 "+3.3V" V 3165 1478 50  0000 L CNN
F 2 "" H 3150 1350 50  0001 C CNN
F 3 "" H 3150 1350 50  0001 C CNN
	1    3150 1350
	1    0    0    -1  
$EndComp
Wire Wire Line
	3150 1350 3150 1475
Wire Wire Line
	3150 1825 3075 1825
Wire Wire Line
	1675 2025 1600 2025
Wire Wire Line
	1600 2025 1600 1925
Wire Wire Line
	1600 1925 1675 1925
Wire Wire Line
	1600 1925 1600 1475
Wire Wire Line
	1600 1475 3150 1475
Connection ~ 1600 1925
Wire Wire Line
	3150 1475 3150 1825
$Comp
L power:GND #PWR?
U 1 1 61530877
P 3150 2700
AR Path="/61530877" Ref="#PWR?"  Part="1" 
AR Path="/6151130E/61530877" Ref="#PWR?"  Part="1" 
F 0 "#PWR?" H 3150 2450 50  0001 C CNN
F 1 "GND" H 3155 2527 50  0000 C CNN
F 2 "" H 3150 2700 50  0001 C CNN
F 3 "" H 3150 2700 50  0001 C CNN
	1    3150 2700
	1    0    0    -1  
$EndComp
Wire Wire Line
	3075 2625 3150 2625
Wire Wire Line
	3150 2625 3150 2700
Text GLabel 1600 2325 0    50   Input ~ 0
SCK
Text GLabel 1600 2225 0    50   Input ~ 0
CSN
Text GLabel 1600 2425 0    50   Input ~ 0
MOSI
Text GLabel 1600 2525 0    50   Input ~ 0
MISO
Wire Wire Line
	1600 2225 1675 2225
Wire Wire Line
	1600 2325 1675 2325
Wire Wire Line
	1600 2425 1675 2425
Wire Wire Line
	1600 2525 1675 2525
$Comp
L Device:C C?
U 1 1 61530889
P 3150 2225
F 0 "C?" H 3175 2375 50  0000 L CNN
F 1 "C0603C105K8PACTU" V 3350 1925 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 3188 2075 50  0001 C CNN
F 3 "https://br.mouser.com/datasheet/2/212/KEM_C1006_X5R_SMD-1103249.pdf" H 3150 2225 50  0001 C CNN
F 4 "CAP CER 1UF 10% 10V X5R 0603" H 3825 2150 50  0001 C CNN "Desc"
F 5 "1uF" H 3250 2300 50  0000 C CNN "Comment"
	1    3150 2225
	1    0    0    -1  
$EndComp
Wire Wire Line
	3150 1825 3150 2075
Connection ~ 3150 1825
Wire Wire Line
	3150 2375 3150 2625
Connection ~ 3150 2625
Text GLabel 4900 1875 2    50   Input ~ 0
MISO
Text GLabel 4900 2075 2    50   Input ~ 0
SCK
Text GLabel 4900 2275 2    50   Input ~ 0
CSN
Text GLabel 4900 2175 2    50   Input ~ 0
MOSI
$Comp
L Device:Ferrite_Bead FB?
U 1 1 61530899
P 5525 1975
AR Path="/61530899" Ref="FB?"  Part="1" 
AR Path="/6151130E/61530899" Ref="FB?"  Part="1" 
F 0 "FB?" V 5225 1975 50  0000 C CNN
F 1 "742792653" V 5300 1975 50  0000 C CNN
F 2 "Inductor_SMD:L_1206_3216Metric" V 5455 1975 50  0001 C CNN
F 3 "https://br.mouser.com/datasheet/2/445/742792653-1720685.pdf" H 5525 1975 50  0001 C CNN
F 4 "Ferrite Beads 0603 600R 0.3A" V 5525 1975 50  0001 C CNN "Desc"
F 5 "600R/0.3A" V 5375 1975 50  0000 C CNN "Comment"
	1    5525 1975
	0    1    1    0   
$EndComp
$Comp
L Device:C C?
U 1 1 615308A1
P 5975 2175
F 0 "C?" H 6075 2325 50  0000 L CNN
F 1 "0603B104J250CT" H 6075 2175 50  0000 L CNN
F 2 "Capacitor_SMD:C_0603_1608Metric" H 6013 2025 50  0001 C CNN
F 3 "https://br.mouser.com/datasheet/2/210/WTC_MLCC_General_Purpose-1534899.pdf" H 5975 2175 50  0001 C CNN
F 4 "CAP CER 0.1UF 5% 25V X7R 0603" H 6650 2100 50  0001 C CNN "Desc"
F 5 "100nF" H 6200 2250 50  0000 C CNN "Comment"
	1    5975 2175
	1    0    0    -1  
$EndComp
$Comp
L TC2030-IDC-NL:TC2030-IDC-NL J?
U 1 1 615308AA
P 4525 2175
F 0 "J?" H 4468 2742 50  0000 C CNN
F 1 "TC2030-IDC-NL" H 4468 2651 50  0000 C CNN
F 2 "TAG_TC2030-IDC-NL" H 4525 2175 50  0001 L BNN
F 3 "" H 4525 2175 50  0001 L BNN
F 4 "MS" H 4525 2175 50  0001 L BNN "PARTREV"
F 5 "Tag-Connect" H 4525 2175 50  0001 L BNN "MANUFACTURER"
F 6 "Manufacturer recommendation" H 4525 2175 50  0001 L BNN "STANDARD"
	1    4525 2175
	-1   0    0    -1  
$EndComp
Wire Wire Line
	4825 1875 4900 1875
Wire Wire Line
	4825 2075 4900 2075
Wire Wire Line
	4825 2175 4900 2175
Wire Wire Line
	4825 2275 4900 2275
Wire Wire Line
	5975 2325 5975 2375
Wire Wire Line
	5975 2025 5975 1975
Wire Wire Line
	6800 2375 6800 2175
Wire Wire Line
	6800 1700 6800 1775
Wire Wire Line
	6675 1700 6800 1700
Wire Wire Line
	5975 1975 6450 1975
Wire Wire Line
	5975 2375 6800 2375
$Comp
L Connector:Conn_01x07_Female J?
U 1 1 615308C3
P 1625 4350
F 0 "J?" H 1517 3917 50  0000 C CNN
F 1 "Conn_01x07_Female" H 1517 3916 50  0001 C CNN
F 2 "Connector_PinHeader_2.54mm:PinHeader_1x07_P2.54mm_Horizontal" H 1625 4350 50  0001 C CNN
F 3 "~" H 1625 4350 50  0001 C CNN
	1    1625 4350
	-1   0    0    1   
$EndComp
$Comp
L power:GND #PWR?
U 1 1 615308C9
P 1900 4725
F 0 "#PWR?" H 1900 4475 50  0001 C CNN
F 1 "GND" H 1905 4552 50  0000 C CNN
F 2 "" H 1900 4725 50  0001 C CNN
F 3 "" H 1900 4725 50  0001 C CNN
	1    1900 4725
	1    0    0    -1  
$EndComp
Wire Wire Line
	1900 4650 1825 4650
Wire Wire Line
	1900 4650 1900 4725
Text GLabel 6675 1700 0    50   Input ~ 0
RESN
Text GLabel 1900 4550 2    50   Input ~ 0
RESN
Text GLabel 1900 4450 2    50   Input ~ 0
MOSI
Text GLabel 1900 4350 2    50   Input ~ 0
SCK
Text GLabel 1900 4250 2    50   Input ~ 0
MISO
$Comp
L power:+3.3V #PWR?
U 1 1 615308D6
P 1900 3975
F 0 "#PWR?" H 1900 3825 50  0001 C CNN
F 1 "+3.3V" V 1915 4103 50  0000 L CNN
F 2 "" H 1900 3975 50  0001 C CNN
F 3 "" H 1900 3975 50  0001 C CNN
	1    1900 3975
	1    0    0    -1  
$EndComp
Wire Wire Line
	1825 4050 1900 4050
Wire Wire Line
	1900 4050 1900 3975
Wire Wire Line
	1825 4150 1900 4150
Wire Wire Line
	1825 4250 1900 4250
Wire Wire Line
	1825 4350 1900 4350
Wire Wire Line
	1825 4450 1900 4450
Wire Wire Line
	1825 4550 1900 4550
Text GLabel 1900 4150 2    50   Input ~ 0
CSN
$Comp
L 1734592-8:1734592-8 J?
U 1 1 615308E8
P 3725 4500
F 0 "J?" H 3925 3775 50  0000 L CNN
F 1 "1734592-8" H 3575 3850 50  0000 L CNN
F 2 "TE_1734592-8" H 3725 4500 50  0001 L BNN
F 3 "" H 3725 4500 50  0001 L BNN
F 4 "Manufacturer Recommendations" H 3725 4500 50  0001 L BNN "STANDARD"
F 5 "TE Connectivity" H 3725 4500 50  0001 L BNN "MANUFACTURER"
F 6 "2.05mm" H 3725 4500 50  0001 L BNN "MAXIMUM_PACKAGE_HEIGHT"
F 7 "C" H 3725 4500 50  0001 L BNN "PARTREV"
	1    3725 4500
	-1   0    0    1   
$EndComp
$Comp
L power:GND #PWR?
U 1 1 615308EE
P 4300 4975
F 0 "#PWR?" H 4300 4725 50  0001 C CNN
F 1 "GND" H 4305 4802 50  0000 C CNN
F 2 "" H 4300 4975 50  0001 C CNN
F 3 "" H 4300 4975 50  0001 C CNN
	1    4300 4975
	1    0    0    -1  
$EndComp
Wire Wire Line
	4225 4900 4300 4900
Wire Wire Line
	4300 4900 4300 4975
Text GLabel 4300 4700 2    50   Input ~ 0
RESN
Text GLabel 4300 4600 2    50   Input ~ 0
MOSI
Text GLabel 4300 4500 2    50   Input ~ 0
SCK
Text GLabel 4300 4400 2    50   Input ~ 0
MISO
Text GLabel 4300 4300 2    50   Input ~ 0
CSN
$Comp
L power:+3.3V #PWR?
U 1 1 615308FB
P 4325 4100
F 0 "#PWR?" H 4325 3950 50  0001 C CNN
F 1 "+3.3V" V 4340 4228 50  0000 L CNN
F 2 "" H 4325 4100 50  0001 C CNN
F 3 "" H 4325 4100 50  0001 C CNN
	1    4325 4100
	1    0    0    -1  
$EndComp
Wire Wire Line
	4225 4200 4325 4200
Wire Wire Line
	4325 4200 4325 4100
Wire Wire Line
	4300 4300 4225 4300
Wire Wire Line
	4225 4400 4300 4400
Wire Wire Line
	4225 4500 4300 4500
Wire Wire Line
	4225 4600 4300 4600
Wire Wire Line
	4225 4700 4300 4700
Connection ~ 3150 1475
$Comp
L Transistor_BJT:DTC113Z Q?
U 1 1 61530909
P 6700 1975
F 0 "Q?" H 6888 2021 50  0000 L CNN
F 1 "DTC113ZUAT106" H 6888 1930 50  0000 L CNN
F 2 "Package_TO_SOT_SMD:SOT-323_SC-70" H 6700 1975 50  0001 L CNN
F 3 "" H 6700 1975 50  0001 L CNN
	1    6700 1975
	1    0    0    -1  
$EndComp
Connection ~ 5975 1975
Connection ~ 5975 2375
Wire Wire Line
	4825 1975 5375 1975
Wire Wire Line
	5675 1975 5975 1975
Wire Wire Line
	4825 2375 5975 2375
$Comp
L power:GND #PWR?
U 1 1 615461D6
P 5975 2450
AR Path="/615461D6" Ref="#PWR?"  Part="1" 
AR Path="/6151130E/615461D6" Ref="#PWR?"  Part="1" 
F 0 "#PWR?" H 5975 2200 50  0001 C CNN
F 1 "GND" H 5980 2277 50  0000 C CNN
F 2 "" H 5975 2450 50  0001 C CNN
F 3 "" H 5975 2450 50  0001 C CNN
	1    5975 2450
	1    0    0    -1  
$EndComp
Wire Wire Line
	5975 2450 5975 2375
$EndSCHEMATC