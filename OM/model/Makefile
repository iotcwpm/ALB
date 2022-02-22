
all: full

.PHONY: all clean

full_20220222-aa.tar.xz:
	wget https://github.com/iotcwpm/ALB/releases/download/v3.0.20220222/full_20220222-aa.tar.xz
 
full_20220222-ab.tar.xz:
	wget https://github.com/iotcwpm/ALB/releases/download/v3.0.20220222/full_20220222-ab.tar.xz
 
full_20220222-ac.tar.xz:
	wget https://github.com/iotcwpm/ALB/releases/download/v3.0.20220222/full_20220222-ac.tar.xz
 
full_20220222-ad.tar.xz:
	wget https://github.com/iotcwpm/ALB/releases/download/v3.0.20220222/full_20220222-ad.tar.xz
 
full_20220222-ae.tar.xz:
	wget https://github.com/iotcwpm/ALB/releases/download/v3.0.20220222/full_20220222-ae.tar.xz
 
full_20220222-af.tar.xz:
	wget https://github.com/iotcwpm/ALB/releases/download/v3.0.20220222/full_20220222-af.tar.xz
 
full.tar.xz: full_20220222-aa.tar.xz full_20220222-ab.tar.xz full_20220222-ac.tar.xz full_20220222-ad.tar.xz full_20220222-ae.tar.xz full_20220222-af.tar.xz
	cat full_20220222-*.tar.xz > full.tar.xz

full: full.tar.xz
	tar xvf full.tar.xz

clean:
	rm -f *.tar.xz

cleanall: clean
	rm -rf full/*/
