remote_host = cloud
remote_dir = ~/sites/stats/public_html/cautioning-canary
remote_dest = $(remote_host):$(remote_dir)

.PHONY: clean html upload serve

serve:
	Rscript -e "servr::httw(dir = '_site', port = 7000)"

# This is handled by targets now
# html:
# 	Rscript -e "rmarkdown::render_site(encoding = 'UTF-8')"

clean:
	Rscript -e "rmarkdown::clean_site()"

upload:
	rsync -crvP --delete _site/ $(remote_dest)
