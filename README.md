# Rapid.Generics

##### How to use
* Include Rapid.Generics.pas unit into your project
* Replace `Generics.Collections` and `Generics.Defaults` units to `Rapid.Generics` in your `uses` code sections
 
##### Note
Do not use generics in the \*.dpr-files for XE8 or XE10 Seattle compilers: _QC#103798_.

##### TRapidDictionary/TRapidObjectDictionary
Rapid "inline" `TDictionary`/`TObjectDictionary` equivalents with default hash codes and comparers

##### Benchmarks: dictionaries
![](https://github.com/d-mozulyov/Rapid.Generics/raw/master/data/Dictionaries.png)

##### Benchmarks: containers
![](https://github.com/d-mozulyov/Rapid.Generics/raw/master/data/Containers.png)

##### Benchmarks: sortings
![](https://github.com/d-mozulyov/Rapid.Generics/raw/master/data/Sortings.png)