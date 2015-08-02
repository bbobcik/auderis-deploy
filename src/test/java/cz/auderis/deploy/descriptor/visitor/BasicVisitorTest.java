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

package cz.auderis.deploy.descriptor.visitor;

import cz.auderis.deploy.descriptor.Deployment;
import cz.auderis.deploy.descriptor.bean.NormalBean;
import cz.auderis.deploy.descriptor.bean.StandaloneListBean;
import cz.auderis.deploy.descriptor.bean.StandaloneMapBean;
import cz.auderis.deploy.descriptor.bean.StandaloneSetBean;
import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.dependency.PropertyInjectionElement;
import cz.auderis.deploy.descriptor.initializer.CollectionItemElement;
import cz.auderis.deploy.descriptor.initializer.ConstructorArgumentElement;
import cz.auderis.deploy.descriptor.initializer.ConstructorElement;
import cz.auderis.deploy.descriptor.initializer.MapEntryElement;
import cz.auderis.deploy.descriptor.initializer.MapKeyElement;
import cz.auderis.deploy.descriptor.initializer.MapValueElement;
import cz.auderis.deploy.descriptor.initializer.PropertyElement;
import cz.auderis.test.category.UnitTest;
import org.junit.Before;
import org.junit.Test;
import org.junit.experimental.categories.Category;

import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Source;

import static cz.auderis.deploy.XmlSupport.createValidatingParser;
import static cz.auderis.deploy.XmlSupport.xml;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

public class BasicVisitorTest {

	private Unmarshaller xmlParser;

	@Before
	public void initializeParser() throws Exception {
		xmlParser = createValidatingParser();
	}

	@Test
	@Category(UnitTest.class)
	public void shouldVisitTopLevelElements() throws Exception {
		// Given
		final Source xml = xml(getClass(), "visitor-test-1.xml");
		final Deployment deployment = (Deployment) xmlParser.unmarshal(xml);
		final DeploymentVisitor visitor = mock(DeploymentVisitor.class);
		final VisitorContext context = new DefaultVisitorContext();

		// When
		deployment.accept(visitor, context);

		// Then
		verify(visitor, times(1)).visitDeployment(deployment);
		verify(visitor, times(6)).visitNormalBean(any(NormalBean.class));
		verify(visitor, times(1)).visitListBean(any(StandaloneListBean.class));
		verify(visitor, times(1)).visitSetBean(any(StandaloneSetBean.class));
		verify(visitor, times(1)).visitMapBean(any(StandaloneMapBean.class));
		verifyNoMoreInteractions(visitor);
	}

	@Test
	@Category(UnitTest.class)
	public void shouldVisitAllElementsWithStructuralVisitor() throws Exception {
		// Given
		final Source xml = xml(getClass(), "visitor-test-1.xml");
		final Deployment deployment = (Deployment) xmlParser.unmarshal(xml);
		final DeploymentStructureVisitor visitor = mock(DeploymentStructureVisitor.class);
		final VisitorContext context = new DefaultVisitorContext();

		// When
		deployment.accept(visitor, context);

		// Then
		verify(visitor, times(1)).visitDeployment(deployment);
		verify(visitor, times(6)).visitNormalBean(any(NormalBean.class));
		verify(visitor, times(1)).visitListBean(any(StandaloneListBean.class));
		verify(visitor, times(1)).visitSetBean(any(StandaloneSetBean.class));
		verify(visitor, times(1)).visitMapBean(any(StandaloneMapBean.class));
		//
		verify(visitor, times(12)).visitBeanPropertyDefinition(any(PropertyElement.class));
		verify(visitor, times(2)).visitBeanConstructor(any(ConstructorElement.class));
		verify(visitor, times(4)).visitBeanConstructorArgument(any(ConstructorArgumentElement.class));
		verify(visitor, times(6)).visitCollectionItem(any(CollectionItemElement.class));
		verify(visitor, times(2)).visitMapEntry(any(MapEntryElement.class));
		verify(visitor, times(2)).visitMapKey(any(MapKeyElement.class));
		verify(visitor, times(2)).visitMapValue(any(MapValueElement.class));
		//
		verify(visitor, times(6)).visitBeanInjection(any(BeanInjectionElement.class));
		verify(visitor, times(6)).visitPropertyInjection(any(PropertyInjectionElement.class));
		verify(visitor, times(10)).visitStringValue(anyString());
		//
		verifyNoMoreInteractions(visitor);
	}

}
