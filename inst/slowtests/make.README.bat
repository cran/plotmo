@rem Create README.html from README.md
"C:\PROGRA~1\R\R-4.2.0\bin\x64\R.exe" CMD BATCH --quiet --vanilla make.README.R
cat make.readme.Rout
rm -f make.readme.Rout
