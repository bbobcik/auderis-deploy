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

import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.dependency.ExtraDependency;
import cz.auderis.deploy.descriptor.dependency.PropertyInjectionElement;
import cz.auderis.deploy.descriptor.initializer.CollectionItemElement;
import cz.auderis.deploy.descriptor.initializer.ConstructorArgumentElement;
import cz.auderis.deploy.descriptor.initializer.ConstructorElement;
import cz.auderis.deploy.descriptor.initializer.MapEntryElement;
import cz.auderis.deploy.descriptor.initializer.MapKeyElement;
import cz.auderis.deploy.descriptor.initializer.MapValueElement;
import cz.auderis.deploy.descriptor.initializer.PropertyElement;

public class DeploymentStructureVisitorAdapter extends DeploymentVisitorAdapter implements DeploymentStructureVisitor {

	@Override
	public void visitBeanPropertyDefinition(PropertyElement property) {
		// No operation
	}

	@Override
	public void visitBeanConstructor(ConstructorElement constructor) {
		// No operation
	}

	@Override
	public void visitBeanConstructorArgument(ConstructorArgumentElement constructorArgument) {
		// No operation
	}

	@Override
	public void visitCollectionItem(CollectionItemElement collectionItem) {
		// No operation
	}

	@Override
	public void visitMapEntry(MapEntryElement mapEntry) {
		// No operation
	}

	@Override
	public void visitMapKey(MapKeyElement mapKey) {
		// No operation
	}

	@Override
	public void visitMapValue(MapValueElement mapValue) {
		// No operation
	}

	@Override
	public void visitBeanInjection(BeanInjectionElement beanInjection) {
		// No operation
	}

	@Override
	public void visitPropertyInjection(PropertyInjectionElement propertyInjection) {
		// No operation
	}

	@Override
	public void visitExtraDependency(ExtraDependency extraDependency) {
		// No operation
	}

	@Override
	public void visitStringValue(String item) {
		// No operation
	}

	@Override
	public void visitUnknownValue(Object item) {
		// No operation
	}

}
