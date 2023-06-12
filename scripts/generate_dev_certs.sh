#!/bin/bash
DST=priv/_dev_certs/
CFG=scripts/
SERVER=braidnet.local
CA=${SERVER}.CA

mkdir ${DST}
rm -rf $DST}*

# CA self signed
openssl genrsa -out ${DST}${CA}.key 4096
openssl req -new -x509 -config ${CFG}${CA}.cfg -key ${DST}${CA}.key -out ${DST}${CA}.pem

gen_issued_cert()
{
    NAME=$1
    KEY_LEN=$2
    ISSUER=$3
    openssl genrsa -out ${DST}${NAME}.key 2048
    openssl req -new -config ${CFG}${NAME}.cfg -key ${DST}${NAME}.key -out ${DST}${NAME}.csr
    openssl x509 -req -extfile ${CFG}${NAME}_v3_ext.cfg \
        -in ${DST}${NAME}.csr -CA ${DST}${ISSUER}.pem -CAkey ${DST}${ISSUER}.key \
        -CAcreateserial -out ${DST}${NAME}.pem
}

# End Entity
gen_issued_cert ${SERVER} 2048 ${CA}
