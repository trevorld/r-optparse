desc "Build files for packaging"
task :default do
    sh 'Rscript -e "devtools::document()"'
    sh 'Rscript -e "knitr::knit(\"README.Rrst\")"'
    sh 'pandoc -o README.md README.rst'
    sh 'rst2html README.rst README.html'
    sh 'Rscript -e "pkgdown::build_site()"'
end
