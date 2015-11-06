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

package cz.auderis.deploy.parser;

import cz.auderis.deploy.descriptor.Deployment;
import cz.auderis.deploy.descriptor.bean.ExternalBundleBean;
import cz.auderis.deploy.descriptor.bean.NormalBean;
import cz.auderis.deploy.descriptor.bean.StandaloneListBean;
import cz.auderis.deploy.descriptor.bean.StandaloneMapBean;
import cz.auderis.deploy.descriptor.bean.StandaloneSetBean;
import cz.auderis.deploy.descriptor.visitor.DefaultVisitorContext;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;
import cz.auderis.test.category.UnitTest;
import junitparams.JUnitParamsRunner;
import junitparams.Parameters;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.xml.sax.SAXException;

import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static cz.auderis.deploy.descriptor.bean.BeanLifecycleStage.EAGER_CONFLICT_FAILURE;
import static cz.auderis.deploy.descriptor.bean.BeanLifecycleStage.LAZY_CONFLICT_FAILURE;
import static cz.auderis.deploy.matcher.BeanMatchers.hasLifecycleStage;
import static cz.auderis.deploy.matcher.BeanMatchers.locatedOnlyBeans;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

@RunWith(JUnitParamsRunner.class)
public class BeanLocatorVisitorTest {

	private static final String DEFAULT_NAME = "x1";

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	VisitorContext context;
	BeanLocatorVisitor beanLocator;
	Unmarshaller xmlParser;

	@Before
	public void initializeLocator() throws Exception {
		context = new DefaultVisitorContext();
		beanLocator = new BeanLocatorVisitor(context);
		xmlParser = createValidatingParser();
	}

	@Test
	@Category(UnitTest.class)
	public void shouldLocateNonConflictingBeans() throws Exception {
		// Given
		final Source xml = xmlResource("simpleAllBeanTypes.xml");
		final Deployment deployment = (Deployment) xmlParser.unmarshal(xml);

		// When
		deployment.accept(beanLocator, context);

		// Then
		assertThat(beanLocator, locatedOnlyBeans("normalBean", "listBean", "setBean", "mapBean", "bundleBean"));
		assertThat(beanLocator.getBeanByName("normalBean"), is(instanceOf(NormalBean.class)));
		assertThat(beanLocator.getBeanByName("listBean"), is(instanceOf(StandaloneListBean.class)));
		assertThat(beanLocator.getBeanByName("setBean"), is(instanceOf(StandaloneSetBean.class)));
		assertThat(beanLocator.getBeanByName("mapBean"), is(instanceOf(StandaloneMapBean.class)));
		assertThat(beanLocator.getBeanByName("bundleBean"), is(instanceOf(ExternalBundleBean.class)));
	}

	@Test
	@Category(UnitTest.class)
	@Parameters(method = "beanTypePairs")
	public void shouldNotAllowOverrideOfBeanType(String baseType, String overrideType) throws Exception {
		// Given
		expectedException.expect(BeanDefinitionException.class);
		expectedException.expectMessage(containsString("incompatible type"));
		expectedException.reportMissingExceptionWithMessage("bean type " + baseType + " was overridden by type " + overrideType);
		final Source xml = deployment(emptyBean(baseType), emptyBean(overrideType));
		final Deployment deployment = (Deployment) xmlParser.unmarshal(xml);

		// When / Then - should cause exception
		deployment.accept(beanLocator, context);
	}

	@Test
	@Category(UnitTest.class)
	@Parameters(method = "beanTypes")
	public void shouldForbidBeanOverrideWhenFailConflictMode(String beanType) throws Exception {
		// Given
		expectedException.expect(BeanDefinitionException.class);
		expectedException.expectMessage(containsString("forbidden"));
		expectedException.reportMissingExceptionWithMessage("bean type " + beanType + " allowed override");
		final Source xml = deployment(emptyBean(beanType, "conflict=\"Fail\""), emptyBean(beanType));
		final Deployment deployment = (Deployment) xmlParser.unmarshal(xml);

		// When / Then - should cause exception
		deployment.accept(beanLocator, context);
	}

	@Test
	@Category(UnitTest.class)
	@Parameters(method = "beanTypes")
	public void shouldMarkOriginalBeanAsInvalidWhenFailConflictMode(String beanType) throws Exception {
		// Given
		final Source xml = deployment(emptyBean(beanType, "conflict=\"Fail\""), emptyBean(beanType));
		final Deployment deployment = (Deployment) xmlParser.unmarshal(xml);

		// When
		try {
			deployment.accept(beanLocator, context);
		} catch (Exception e) {
			// Exception disregarded
		}

		// Then
		assertThat(beanLocator, locatedOnlyBeans(DEFAULT_NAME));
		assertThat(beanLocator.getBeanByName(DEFAULT_NAME), hasLifecycleStage(EAGER_CONFLICT_FAILURE));
	}

	@Test
	@Category(UnitTest.class)
	@Parameters(method = "beanTypes")
	public void shouldMarkOverriddenBeanAsInvalidWhenLazyFailConflictMode(String beanType) throws Exception {
		// Given
		final Source xml = deployment(emptyBean(beanType, "conflict=\"FailOnUse\""), emptyBean(beanType));
		final Deployment deployment = (Deployment) xmlParser.unmarshal(xml);

		// When
		deployment.accept(beanLocator, context);

		// Then
		assertThat(beanLocator, locatedOnlyBeans(DEFAULT_NAME));
		assertThat(beanLocator.getBeanByName(DEFAULT_NAME), hasLifecycleStage(LAZY_CONFLICT_FAILURE));
	}


	private static Source xmlResource(String resourceName) throws SAXException {
		return xml(BeanLocatorVisitorTest.class, resourceName);
	}

	private static Source deployment(String... parts) throws SAXException {
		final StringBuilder str = new StringBuilder(256);
		str.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
		str.append("<deployment>");
		for (final String part : parts) {
			str.append(part);
		}
		str.append("</deployment>");
		return xml(str.toString());
	}

	private static String emptyBean(String type, String... attributes) {
		final StringBuilder str = new StringBuilder(64);
		str.append('<').append(type).append(' ');
		boolean nameAttrUsed = false;
		boolean classAttrUsed = false;
		for (final String attr : attributes) {
			str.append(attr).append(' ');
			if (attr.startsWith("class=")) {
				classAttrUsed = true;
			} else if (attr.startsWith("name=")) {
				nameAttrUsed = true;
			}
		}
		if (!nameAttrUsed) {
			str.append("name=\"").append(DEFAULT_NAME).append("\" ");
		}
		if (!classAttrUsed && type.equals("bean")) {
			str.append("class=\"java.lang.Void\" ");
		}
		str.append("/>");
		return str.toString();
	}

	private String[] beanTypes() {
		final String[] types = { "bean", "list", "set", "map", "propertyBundle" };
		return types;
	}

	private String[] beanTypePairs() {
		final String[] types = beanTypes();
		final int typeCount = types.length;
		final String[] pairs = new String[typeCount * (typeCount - 1)];
		int pairIdx = 0;
		for (final String t1 : types) {
			for (final String t2 : types) {
				if (!t1.equals(t2)) {
					pairs[pairIdx] = t1 + ", " + t2;
					++pairIdx;
				}
			}
		}
		return pairs;
	}

}
