#IDIR = -I /n/fs/fac/vivek/codeen/comon/
CC=gcc
CFLAGS = -lpthread
LIBS = -L/usr/lib -lcurl -L/usr/kerberos/lib -lssl -lcrypto -lgssapi_krb5 -lkrb5 -lcom_err -lk5crypto -lresolv -ldl -lz -lgssapi_krb5 -lkrb5 -lk5crypto -lcom_err -lresolv -L/usr/kerberos/lib -lidn -lssl -lcrypto -lssl -lcrypto -lgssapi_krb5 -lkrb5 -lcom_err -lk5crypto -lresolv -ldl -lz -lz


get.o: get.c 
	$(CC)  -c  get.c

testget: get.o
	gcc $(LIBS)  $(CFLAGS) get.o  -o testget testget.c

clean: 
	rm -rf *.o a.out