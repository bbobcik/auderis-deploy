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

package cz.auderis.deploy.descriptor;


import cz.auderis.deploy.descriptor.bean.NormalBean;
import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.initializer.PropertyElement;
import cz.auderis.deploy.descriptor.producer.FactoryDefinition;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLFilterImpl;
import org.xml.sax.helpers.XMLReaderFactory;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.SchemaOutputResolver;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public final class DescriptorParserSupport {

	private static final List<Class<?>> JAXB_PACKAGE_REPRESENTING_CLASSES = getRepresentingClasses();

	public static String getJaxbContextPackages() {
		final StringBuilder str = new StringBuilder(512);
		for (final Class<?> repClass : JAXB_PACKAGE_REPRESENTING_CLASSES) {
			final Package pkg = repClass.getPackage();
			final String pkgName = pkg.getName();
			if (0 != str.length()) {
				str.append(':');
			}
			str.append(pkgName);
		}
		return str.toString();
	}

	public static Source filterSourceNamespace(InputSource src) throws SAXException {
		if (null == src) {
			throw new NullPointerException();
		}
		// Create filter for XML namespace processing
		final XMLReader parentReader = XMLReaderFactory.createXMLReader();
		final DescriptorParserSupport.NamespaceFilter filter = new DescriptorParserSupport.NamespaceFilter(Deployment.SCHEMA_URI, true);
		filter.setParent(parentReader);
		// Create filtered SAX source
		final SAXSource saxSource = new SAXSource(filter, src);
		return saxSource;
	}

	public static Schema createXsdValidationSchema(JAXBContext context) throws DescriptorParsingException {
		final URL schemaURL = Deployment.class.getResource(Deployment.SCHEMA_LOCATION);
		if (null == schemaURL) {
			throw new DescriptorParsingException("Cannot locate deployment XML schema");
		}
		try {
			final SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
			final Schema schema = factory.newSchema(schemaURL);
			return schema;
		} catch (SAXException e) {
			throw new DescriptorParsingException("Unable to create deployment XML schema", e);
		}
	}

	public static Schema createJaxbValidationSchema(JAXBContext context) throws DescriptorParsingException {
		try {
			final List<ByteArrayOutputStream> outputStreams = new ArrayList<ByteArrayOutputStream>(8);
			context.generateSchema(new SchemaOutputResolver() {
				@Override
				public Result createOutput(String namespaceUri, String suggestedFileName) throws IOException {
					final ByteArrayOutputStream out = new ByteArrayOutputStream(256);
					outputStreams.add(out);
					final StreamResult streamResult = new StreamResult(out);
					streamResult.setSystemId("");
					return streamResult;
				}
			});
			final Source[] sources = new Source[outputStreams.size()];
			int idx = 0;
			for (final ByteArrayOutputStream outputStream : outputStreams) {
				final byte[] outputBytes = outputStream.toByteArray();
				final ByteArrayInputStream inputStream = new ByteArrayInputStream(outputBytes);
				sources[idx++] = new StreamSource(inputStream);
			}
			final SchemaFactory factory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
			final Schema schema = factory.newSchema(sources);
			return schema;
		} catch (IOException e) {
			throw new DescriptorParsingException("Unable to resolve schema set", e);
		} catch (SAXException e) {
			throw new DescriptorParsingException("Unable to generate validation schema", e);
		}
	}

	public static boolean isStrictParsingEnabled() {
		return !Boolean.getBoolean("auderis.deploy.lenient");
	}

	public static boolean isBlank(String arg) {
		if (null == arg) {
			return true;
		}
		final int len = arg.length();
		for (int i=0; i<len; ++i) {
			final char c = arg.charAt(i);
			if (!Character.isWhitespace(c)) {
				return false;
			}
		}
		return true;
	}

	public static Set<String> recognizedNameSet(String canonicalName, String ... aliases) {
		final Set<String> allNames = new HashSet<String>(1 + aliases.length);
		allNames.add(canonicalName);
		allNames.addAll(Arrays.asList(aliases));
		assert !allNames.contains(null);
		assert !allNames.contains("");
		return Collections.unmodifiableSet(allNames);
	}

	public static <T extends Enum<T> & NamedEnum> T parseEnumByName(String name, Class<T> enumClass) {
		if (isBlank(name)) {
			return null;
		}
		final T[] enumConstants = enumClass.getEnumConstants();
		if (isStrictParsingEnabled()) {
			for (final T enumConstant : enumConstants) {
				if (enumConstant.getCanonicalName().equals(name)) {
					return enumConstant;
				}
			}
		} else {
			final String normalizedName = name.trim().toLowerCase();
			for (final T enumConstant : enumConstants) {
				for (final String enumName : enumConstant.getRecognizedNames()) {
					if (normalizedName.equalsIgnoreCase(enumName)) {
						return enumConstant;
					}
				}
			}
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	private static List<Class<?>> getRepresentingClasses() {
		@SuppressWarnings("RedundantCast") final List<Class<?>> result = Arrays.asList(
				(Class<?>) Deployment.class,
				(Class<?>) NormalBean.class,
				(Class<?>) BeanInjectionElement.class,
				(Class<?>) PropertyElement.class,
				(Class<?>) FactoryDefinition.class
		);
		return Collections.unmodifiableList(result);
	}

	private DescriptorParserSupport() {
		throw new AssertionError();
	}

	public interface NamedEnum {
		String getCanonicalName();
		Set<String> getRecognizedNames();
	}

	public static class NamespaceFilter extends XMLFilterImpl {

		final String usedNamespaceUri;
		final boolean addNamespace;
		boolean addedNamespace;

		public NamespaceFilter(String namespaceUri, boolean addNamespace) {
			super();
			this.addNamespace = addNamespace;
			if (addNamespace) {
				assert null != namespaceUri;
				this.usedNamespaceUri = namespaceUri;
			} else {
				this.usedNamespaceUri = "";
			}
		}

		@Override
		public void startDocument() throws SAXException {
			super.startDocument();
			if (addNamespace) {
				startControlledPrefixMapping();
			}
		}

		@Override
		public void startElement(String uri, String localName, String qName, Attributes attrs) throws SAXException {
			super.startElement(this.usedNamespaceUri, localName, qName, attrs);
		}

		@Override
		public void endElement(String uri, String localName, String qName) throws SAXException {
			super.endElement(this.usedNamespaceUri, localName, qName);
		}

		@Override
		public void startPrefixMapping(String prefix, String url) throws SAXException {
			if (addNamespace) {
				this.startControlledPrefixMapping();
			} else {
				//Remove the namespace, i.e. don´t call startPrefixMapping for parent!
			}
		}

		private void startControlledPrefixMapping() throws SAXException {
			if (addedNamespace || !addNamespace) {
				return;
			}
			// We should add namespace since it is set and has not yet been done.
			super.startPrefixMapping("", this.usedNamespaceUri);
			this.addedNamespace = true;
		}
	}

}
