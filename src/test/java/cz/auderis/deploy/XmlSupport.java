/*
 * Copyright 2015 Boleslav Bobcik - Auderis
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package cz.auderis.deploy;

import cz.auderis.deploy.descriptor.DescriptorParserSupport;
import junitparams.converters.ConversionFailedException;
import junitparams.converters.ParamConverter;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import javax.xml.validation.Schema;
import java.io.InputStream;
import java.io.StringReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class XmlSupport {

	public static Source xml(String xmlText) throws SAXException {
		// Prepare source based on input text
		final StringReader inReader = new StringReader(xmlText);
		final InputSource baseSource = new InputSource(inReader);
		// Create filtered SAX source
		final Source filteredSource = DescriptorParserSupport.filterSourceNamespace(baseSource);
		return filteredSource;
	}

	public static Source xml(Class<?> baseClass, String resourceName) throws SAXException {
		final InputStream resourceStream = baseClass.getResourceAsStream(resourceName);
		final InputSource baseSource = new InputSource(resourceStream);
		final Source filteredSource = DescriptorParserSupport.filterSourceNamespace(baseSource);
		return filteredSource;
	}

	public static String cdata(String text) {
		return "<![CDATA[" + text + "]]>";
	}

	public static Unmarshaller createValidatingParser() throws JAXBException {
		final String packages = DescriptorParserSupport.getJaxbContextPackages();
		final JAXBContext jaxbCtx = JAXBContext.newInstance(packages);
		final Unmarshaller xmlParser = jaxbCtx.createUnmarshaller();
		// Enable validation
		final Schema schema = DescriptorParserSupport.createXsdValidationSchema(jaxbCtx);
		xmlParser.setSchema(schema);
		return xmlParser;
	}

	public static Unmarshaller createLenientParser() throws JAXBException {
		final String packages = DescriptorParserSupport.getJaxbContextPackages();
		final JAXBContext jaxbCtx = JAXBContext.newInstance(packages);
		final Unmarshaller xmlParser = jaxbCtx.createUnmarshaller();
		return xmlParser;
	}

	public static class SourceConverter implements ParamConverter<Source> {
		private static final Pattern WRAP_PATTERN = Pattern.compile("(.*)\\{\\}(.*)");
		private static final Pattern ID_PATTERN = Pattern.compile("[a-z][a-z0-9_-]*");

		@Override
		public Source convert(Object obj, String option) throws ConversionFailedException {
			final String xmlText = prepareXmlText(obj.toString(), option);
			try {
				return xml(xmlText);
			} catch (SAXException e) {
				throw new ConversionFailedException(e.getMessage());
			}
		}

		private static String prepareXmlText(String baseText, String option) {
			if ((null == option) || option.trim().isEmpty()) {
				return baseText;
			}
			final Matcher matcher = WRAP_PATTERN.matcher(option);
			if (matcher.matches()) {
				return matcher.group(1) + baseText + matcher.group(2);
			}
			matcher.usePattern(ID_PATTERN).reset();
			if (matcher.matches()) {
				return "<" + option + '>' + baseText + "</" + option + '>';
			}
			return baseText;
		}
	}

}
