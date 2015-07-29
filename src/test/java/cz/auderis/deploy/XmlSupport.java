package cz.auderis.deploy;

import java.io.Reader;
import java.io.StringReader;

public final class XmlSupport {

	public static Reader xml(String xmlText) {
		return new StringReader(xmlText);
	}




}
