
all: backend frontend

.PHONY: backend
backend:
	cd backend && dune build

.PHONY: frontend
frontend:
	cd frontend && npm run build && npm run deploy

clean:
	cd frontend && npm run clean
	cd backend && dune clean

distclean: clean
	cd frontend && rm -rf node_modules

#should create cocass psql user and database too ?
install:
	- groupadd cocass
	- useradd -b /usr/local/lib/cocass -g cocass cocass
	install backend/_build/default/src/backend.exe /usr/local/bin/cocass
	- rm -rf /usr/local/lib/cocass
	cp -r frontend/dist /usr/local/lib/cocass
	chown cocass:cocass -R /usr/local/lib/cocass
	install cocass.service /etc/systemd/system/
	- mkdir /var/log/cocass
	chown cocass:cocass -R /var/log/cocass
	- rm /var/log/cocass/cocass.*
	systemctl daemon-reload
	systemctl restart cocass
