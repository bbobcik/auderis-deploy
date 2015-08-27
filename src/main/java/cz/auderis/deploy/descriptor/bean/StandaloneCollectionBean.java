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

import javax.xml.bind.annotation.XmlAttribute;

public abstract class StandaloneCollectionBean extends AbstractBean {
	private static final long serialVersionUID = 20150728L;

	@XmlAttribute(name = "mutable", required = false)
	protected boolean mutable;

	protected StandaloneCollectionBean(BeanType beanType) {
		super(beanType);
	}

	public boolean isMutable() {
		return mutable;
	}

}
