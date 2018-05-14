package com.verizon.netsense.utils

/**
 * Created by brefsdal on 4/6/17.
 */
import java.io._
import java.security._
import java.security.cert.X509Certificate
import java.security.spec.PKCS8EncodedKeySpec
import javax.crypto.Cipher
import javax.net.ssl._

import org.bouncycastle.asn1.pkcs.{EncryptedPrivateKeyInfo, PrivateKeyInfo}
import org.bouncycastle.cert.X509CertificateHolder
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openssl.{PEMEncryptedKeyPair, PEMKeyPair, PEMParser}
import org.bouncycastle.openssl.jcajce.{JcaPEMKeyConverter, JceOpenSSLPKCS8DecryptorProviderBuilder, JcePEMDecryptorProviderBuilder}
import org.bouncycastle.pkcs.PKCS8EncryptedPrivateKeyInfo
import org.bouncycastle.pkcs.jcajce.JcePKCSPBEInputDecryptorProviderBuilder

object SslUtils {

  def getSocketFactory(caCrtFile: String, crtFile: String, keyFile: String, password: String): SSLSocketFactory =
    getSocketFactory(new FileReader(caCrtFile), new FileReader(crtFile), new FileReader(keyFile), password: String)

  def getSocketFactory(caCrtFile: Reader, crtFile: Reader, keyFile: Reader, password: String): SSLSocketFactory = {

    try {

      /**
       * Add BouncyCastle as a Security Provider
       */
      Security.addProvider(new BouncyCastleProvider())
      val certificateConverter = new JcaX509CertificateConverter().setProvider("BC")

      /**
       * Load Certificate Authority (CA) certificate
       */
      var reader       = new PEMParser(caCrtFile)
      val caCertHolder = reader.readObject.asInstanceOf[X509CertificateHolder]
      reader.close()

      val caCert = certificateConverter.getCertificate(caCertHolder)

      /**
       * Load client certificate
       */
      reader = new PEMParser(crtFile)

      // PEM file should contain certificate chain of server cert and root cert
      // Read Object as Server Cert
      val certHolder = reader.readObject.asInstanceOf[X509CertificateHolder]
      // Read 2nd Object as Root Cert
      val rootCertHolder = reader.readObject.asInstanceOf[X509CertificateHolder]
      reader.close()
      val cert = certificateConverter.getCertificate(certHolder)
      val rootCert = certificateConverter.getCertificate(rootCertHolder)

      /**
       * Load client private key
       */
      reader = new PEMParser(keyFile)
      val keyObject = reader.readObject()
      reader.close()

//      val f = new File(keyFile)
//      val fis = new FileInputStream(f)
//      val bytes = Stream.continually(fis.read).takeWhile(_ != -1).map(_.toByte).toArray
//      val encryptPKInfo = new EncryptedPrivateKeyInfo(bytes)

//      val cipher = Cipher.getInstance(encryptPKInfo.getAlgName());
//      PBEKeySpec pbeKeySpec = new PBEKeySpec(passwd.toCharArray());
//      SecretKeyFactory secFac = SecretKeyFactory.getInstance(encryptPKInfo.getAlgName());
//      Key pbeKey = secFac.generateSecret(pbeKeySpec);
//      AlgorithmParameters algParams = encryptPKInfo.getAlgParameters();
//      cipher.init(Cipher.DECRYPT_MODE, pbeKey, algParams);
//      KeySpec pkcs8KeySpec = encryptPKInfo.getKeySpec(cipher);
//      KeyFactory kf = KeyFactory.getInstance("RSA");
//      return kf.generatePrivate(pkcs8KeySpec);

      val provider     = new JcePEMDecryptorProviderBuilder().build(password.toCharArray)
      val keyConverter = new JcaPEMKeyConverter().setProvider("BC")

//      val decryptorProvider = new JceOpenSSLPKCS8DecryptorProviderBuilder()
//        .setProvider("BC")
//        .build(password.toCharArray)
      val decryptorProvider = new JcePKCSPBEInputDecryptorProviderBuilder()
        .setProvider("BC")
        .build(password.toCharArray)

      val key: PrivateKey = keyObject match {
        case _: PEMEncryptedKeyPair =>
          keyConverter.getKeyPair(keyObject.asInstanceOf[PEMEncryptedKeyPair].decryptKeyPair(provider)).getPrivate

        case _: PKCS8EncryptedPrivateKeyInfo => {
          val pkcs8Info            = keyObject.asInstanceOf[PKCS8EncryptedPrivateKeyInfo]
          val info: PrivateKeyInfo = pkcs8Info.decryptPrivateKeyInfo(decryptorProvider)
          val keyFact              = KeyFactory.getInstance(info.getPrivateKeyAlgorithm.getAlgorithm.getId, "BC")
          val privateKey           = keyFact.generatePrivate(new PKCS8EncodedKeySpec(info.getEncoded))
          privateKey
        }
        case _ => keyConverter.getKeyPair(keyObject.asInstanceOf[PEMKeyPair]).getPrivate
      }

      /**
       * CA certificate is used to authenticate server
       */
      val caKeyStore = KeyStore.getInstance(KeyStore.getDefaultType)
      caKeyStore.load(null, null)
      caKeyStore.setCertificateEntry("ca-certificate", caCert)

      val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      trustManagerFactory.init(caKeyStore)

      /**
       * Client key and certificates are sent to server so it can authenticate the client
       */
      val certificateArray = List(cert, rootCert)

      val clientKeyStore: KeyStore = KeyStore.getInstance(KeyStore.getDefaultType)
      clientKeyStore.load(null, null)
      clientKeyStore.setCertificateEntry("certificate", cert)
      clientKeyStore.setKeyEntry("private-key", key, password.toCharArray, certificateArray.toArray)

      val keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
      keyManagerFactory.init(clientKeyStore, password.toCharArray)

      /**
       * Create SSL socket factory
       */
      val context = SSLContext.getInstance("TLSv1.2")
      context.init(keyManagerFactory.getKeyManagers, trustManagerFactory.getTrustManagers, null)

      /**
       * Return the newly created socket factory object
       */
      context.getSocketFactory

    } catch {
      case e: Exception =>
        e.printStackTrace()
        null
    }
  }

}
