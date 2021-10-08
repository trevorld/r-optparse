desc "Build files for packaging"
task :default do
    sh 'Rscript -e "devtools::document()"'
    sh 'Rscript -e "knitr::knit(\"README.Rrst\")"'
    sh 'rst2html README.rst README.html'
    sh 'pandoc -t markdown_strict -o README.md README.rst'
    sh 'Rscript -e "pkgdown::build_site()"'
end
