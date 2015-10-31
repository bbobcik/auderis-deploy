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

package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.dependency.PropertyInjectionElement;
import cz.auderis.deploy.descriptor.initializer.InitializerContentType;
import cz.auderis.deploy.descriptor.initializer.MapEntryElement;
import cz.auderis.test.category.SanityTest;
import cz.auderis.test.category.UnitTest;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.ExpectedException;
import org.xml.sax.SAXException;

import javax.xml.bind.UnmarshalException;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;
import java.util.List;

import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public class MapBeanParsingTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private static final String BEAN_START = "<map name=\"x\">";
	private static final String BEAN_END = "</map>";

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldRequireNameAttribute() throws Exception {
		// Given
		final Source xml = xml("<map></map>");
		expectedException.expect(UnmarshalException.class);
		expectedException.reportMissingExceptionWithMessage("Map bean without name attribute was parsed");
		// When / Then
		xmlParser.unmarshal(xml);
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanName() throws Exception {
		// Given
		final Source xml = xml("<map name=\"xyz\" />");
		// When
		final StandaloneMapBean bean = (StandaloneMapBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getName(), is("xyz"));
		assertThat(bean.getBeanType(), is(BeanType.MAP));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseKeyClass() throws Exception {
		// Given
		final Source xml = xml("<map name=\"xyz\" keyClass=\"com.test.Type\"></map>");
		// When
		final StandaloneMapBean bean = (StandaloneMapBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getKeyClassName(), is("com.test.Type"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseValueClass() throws Exception {
		// Given
		final Source xml = xml("<map name=\"xyz\" valueClass=\"com.test.Type\"></map>");
		// When
		final StandaloneMapBean bean = (StandaloneMapBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getValueClassName(), is("com.test.Type"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseTextInjectionToKey() throws Exception {
		// Given
		final Source xml = mapWithEntries("ABCD9876x", "9876ZYXa");
		// When
		final StandaloneMapBean bean = (StandaloneMapBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getEntries(), hasSize(1));
		final MapEntryElement entry = bean.getEntries().get(0);
		assertThat(entry.getKeyElement().getContentType(), is(InitializerContentType.TEXT));
		final List<Object> keyContents = entry.getKeyElement().getContents();
		assertThat(keyContents, hasSize(1));
		assertThat(keyContents.get(0), is((Object) "ABCD9876x"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseTextInjectionToValue() throws Exception {
		// Given
		final Source xml = mapWithEntries("ABCD9876x", "9876ZYXa");
		// When
		final StandaloneMapBean bean = (StandaloneMapBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getEntries(), hasSize(1));
		final MapEntryElement entry = bean.getEntries().get(0);
		assertThat(entry.getValueElement().getContentType(), is(InitializerContentType.TEXT));
		final List<Object> valueContents = entry.getValueElement().getContents();
		assertThat(valueContents, hasSize(1));
		assertThat(valueContents.get(0), is((Object) "9876ZYXa"));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanInjectionToKey() throws Exception {
		// Given
		final Source xml = mapWithEntries("<inject bean=\"klm\" />", "9876ZYXa");
		// When
		final StandaloneMapBean bean = (StandaloneMapBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getEntries(), hasSize(1));
		final MapEntryElement entry = bean.getEntries().get(0);
		assertThat(entry.getKeyElement().getContentType(), is(InitializerContentType.BEAN));
		final List<Object> keyContents = entry.getKeyElement().getContents();
		assertThat(keyContents, hasSize(1));
		assertThat(keyContents.get(0), instanceOf(BeanInjectionElement.class));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanInjectionToValue() throws Exception {
		// Given
		final Source xml = mapWithEntries("ABCD9876x", "<inject bean=\"opq\" />");
		// When
		final StandaloneMapBean bean = (StandaloneMapBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getEntries(), hasSize(1));
		final MapEntryElement entry = bean.getEntries().get(0);
		assertThat(entry.getValueElement().getContentType(), is(InitializerContentType.BEAN));
		final List<Object> valueContents = entry.getValueElement().getContents();
		assertThat(valueContents, hasSize(1));
		assertThat(valueContents.get(0), instanceOf(BeanInjectionElement.class));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanPropertyInjectionToKey() throws Exception {
		// Given
		final Source xml = mapWithEntries("<injectProperty bean=\"klm\" property=\"rst\" />", "9876ZYXa");
		// When
		final StandaloneMapBean bean = (StandaloneMapBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getEntries(), hasSize(1));
		final MapEntryElement entry = bean.getEntries().get(0);
		assertThat(entry.getKeyElement().getContentType(), is(InitializerContentType.BEAN_PROPERTY));
		final List<Object> keyContents = entry.getKeyElement().getContents();
		assertThat(keyContents, hasSize(1));
		assertThat(keyContents.get(0), instanceOf(PropertyInjectionElement.class));
	}

	@Test
	@Category(UnitTest.class)
	public void shouldParseBeanPropertyInjectionToValue() throws Exception {
		// Given
		final Source xml = mapWithEntries("ABCD9876x", "<injectProperty bean=\"opq\" property=\"fgh\" />");
		// When
		final StandaloneMapBean bean = (StandaloneMapBean) xmlParser.unmarshal(xml);
		// Then
		assertThat(bean.getEntries(), hasSize(1));
		final MapEntryElement entry = bean.getEntries().get(0);
		assertThat(entry.getValueElement().getContentType(), is(InitializerContentType.BEAN_PROPERTY));
		final List<Object> valueContents = entry.getValueElement().getContents();
		assertThat(valueContents, hasSize(1));
		assertThat(valueContents.get(0), instanceOf(PropertyInjectionElement.class));
	}

	private static Source mapWithEntries(String... keysAndValues) throws SAXException {
		final StringBuilder str = new StringBuilder(2048);
		str.append(BEAN_START);
		for (int i = 0; i+1 < keysAndValues.length; i += 2) {
			final String key = keysAndValues[i];
			final String value = keysAndValues[i + 1];
			str.append("<entry><key>").append(key).append("</key>");
			str.append("<value>").append(value).append("</value></entry>");
		}
		str.append(BEAN_END);
		final Source result = xml(str.toString());
		return result;
	}

}
