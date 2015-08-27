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

public interface DeploymentStructureVisitor extends DeploymentVisitor {

	void visitBeanPropertyDefinition(PropertyElement property);

	void visitBeanConstructor(ConstructorElement constructor);

	void visitBeanConstructorArgument(ConstructorArgumentElement constructorArgument);

	void visitCollectionItem(CollectionItemElement collectionItem);

	void visitMapEntry(MapEntryElement mapEntry);

	void visitMapKey(MapKeyElement mapKey);

	void visitMapValue(MapValueElement mapValue);

	void visitBeanInjection(BeanInjectionElement beanInjection);

	void visitPropertyInjection(PropertyInjectionElement propertyInjection);

	void visitExtraDependency(ExtraDependency extraDependency);

	void visitStringValue(String item);

	void visitUnknownValue(Object item);

}
