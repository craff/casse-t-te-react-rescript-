
all:
	cd frontend && npm run build && npm run deploy
	cd backend && dune build

clean:
	cd frontend && npm run clean
	cd backend && dune clean

install:
	- groupadd cocass
	- useradd -b /usr/local/lib/cocass -g cocass cocass
	install backend/_build/default/src/backend.exe /usr/local/bin/cocass
	cp -r frontend/dist /usr/local/lib/cocass
	chown cocass:cocass -R /usr/local/lib/cocass
	install cocass.service /etc/systemd/system/
	- mkdir /var/local/cocass
	chown cocass:cocass -R /var/log/cocass
