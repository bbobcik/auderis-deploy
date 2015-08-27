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

import cz.auderis.deploy.XmlSupport;
import cz.auderis.test.category.SanityTest;
import cz.auderis.test.category.UnitTest;
import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import junitparams.converters.ConvertParam;
import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.runner.RunWith;

import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static org.hamcrest.Matchers.hasProperty;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

@RunWith(JUnitParamsRunner.class)
public class BasicBeanParsingTest {

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	@Parameters({
			"<bean name=\"xyz\" class=\"java.lang.Object\" ></bean> | xyz | java.lang.Object"
	})
	public void shouldParseEmptyNormalBean(
			@ConvertParam(XmlSupport.SourceConverter.class) Source xml,
			String expectedName,
			String expectedClass
	) throws Exception {
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(NormalBean.class));
		final NormalBean bean = (NormalBean) parsedObj;
		assertThat(bean, hasProperty("name", is(expectedName)));
		assertThat(bean, hasProperty("beanType", is(BeanType.NORMAL)));
		assertThat(bean, hasProperty("beanClassName", is(expectedClass)));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseEmptyListBean() throws Exception {
		// Given
		final Source xml = xml("<list name=\"list123\"></list>");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(StandaloneListBean.class));
		final StandaloneListBean bean = (StandaloneListBean) parsedObj;
		assertThat(bean, hasProperty("name", is("list123")));
		assertThat(bean, hasProperty("beanType", is(BeanType.LIST)));
		assertThat(bean, hasProperty("beanClassName", is("java.util.List")));
		assertThat(bean, hasProperty("itemClassName", is("java.lang.Object")));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseEmptySetBean() throws Exception {
		// Given
		final Source xml = xml("<set name=\"setABC\"></set>");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(StandaloneSetBean.class));
		final StandaloneSetBean bean = (StandaloneSetBean) parsedObj;
		assertThat(bean, hasProperty("name", is("setABC")));
		assertThat(bean, hasProperty("beanType", is(BeanType.SET)));
		assertThat(bean, hasProperty("beanClassName", is("java.util.Set")));
		assertThat(bean, hasProperty("itemClassName", is("java.lang.Object")));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseEmptyMapBean() throws Exception {
		// Given
		final Source xml = xml("<map name=\"aMap\"></map>");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(StandaloneMapBean.class));
		final StandaloneMapBean bean = (StandaloneMapBean) parsedObj;
		assertThat(bean, hasProperty("name", is("aMap")));
		assertThat(bean, hasProperty("beanType", is(BeanType.MAP)));
		assertThat(bean, hasProperty("beanClassName", is("java.util.Map")));
		assertThat(bean, hasProperty("keyClassName", is("java.lang.String")));
		assertThat(bean, hasProperty("valueClassName", is("java.lang.Object")));
	}

	@Test
	@Category({ UnitTest.class, SanityTest.class })
	public void shouldParseEmptyBundleBean() throws Exception {
		// Given
		final Source xml = xml("<propertyBundle name=\"bundle\"></propertyBundle>");
		// When
		final Object parsedObj = xmlParser.unmarshal(xml);
		// Then
		assertThat(parsedObj, instanceOf(ExternalBundleBean.class));
		final ExternalBundleBean bean = (ExternalBundleBean) parsedObj;
		assertThat(bean, hasProperty("name", is("bundle")));
		assertThat(bean, hasProperty("beanType", is(BeanType.EXTERNAL_BUNDLE)));
		assertThat(bean, hasProperty("beanClassName", is("java.util.ResourceBundle")));
	}

}
