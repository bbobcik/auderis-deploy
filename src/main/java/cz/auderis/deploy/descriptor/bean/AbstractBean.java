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

import cz.auderis.deploy.descriptor.DeploymentEntry;
import cz.auderis.deploy.descriptor.DescriptorParsingException;
import cz.auderis.deploy.descriptor.dependency.DependencyInstance;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlTransient;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isBlank;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isStrictParsingEnabled;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.parseEnumByName;

public abstract class AbstractBean extends DeploymentEntry {
	private static final long serialVersionUID = 20150728L;

	@XmlID
	@XmlAttribute(name = "name", required = true)
	protected String name;

	@XmlTransient
	protected BeanInstantiationMode instantiationMode;

	@XmlTransient
	protected BeanConflictMode conflictResolutionMode;

	@XmlTransient
	protected final BeanType beanType;

	protected final Set<DependencyInstance> dependencies;

	protected AbstractBean(BeanType type) {
		if (null == type) {
			throw new NullPointerException();
		}
		this.beanType = type;
		this.instantiationMode = BeanInstantiationMode.getDefaultMode();
		this.conflictResolutionMode = BeanConflictMode.getDefaultMode();
		this.dependencies = new LinkedHashSet<DependencyInstance>(2);
	}

	protected AbstractBean(String name, BeanType type, BeanInstantiationMode instMode, BeanConflictMode conflictMode) {
		assert null != name;
		assert null != type;
		assert null != instMode;
		assert null != conflictMode;
		this.name = name;
		this.beanType = type;
		this.instantiationMode = instMode;
		this.conflictResolutionMode = conflictMode;
		this.dependencies = new LinkedHashSet<DependencyInstance>(2);
	}

	public String getName() {
		return name;
	}

	public final BeanType getBeanType() {
		return beanType;
	}

	public abstract String getBeanClassName();

	public BeanInstantiationMode getInstantiationMode() {
		return instantiationMode;
	}

	public BeanConflictMode getConflictResolutionMode() {
		return conflictResolutionMode;
	}

	public boolean hasIndividualProperties() {
		return beanType.hasIndividualProperties();
	}

	public Set<DependencyInstance> getDependencies() {
		return Collections.unmodifiableSet(dependencies);
	}

	public void addDependency(DependencyInstance dep) {
		if (null == dep) {
			throw new NullPointerException();
		}
		dependencies.add(dep);
	}

	public void removeDependency(DependencyInstance dep) {
		dependencies.remove(dep);
	}

	@XmlAttribute(name = "mode")
	protected final String getInstantiationModeCode() {
		return instantiationMode.getCanonicalName();
	}

	protected final void setInstantiationModeCode(String instModeCode) {
		final BeanInstantiationMode mode = parseEnumByName(instModeCode, BeanInstantiationMode.class);
		if (null != mode) {
			this.instantiationMode = mode;
		} else if (isBlank(instModeCode) && !isStrictParsingEnabled()) {
			this.instantiationMode = BeanInstantiationMode.getDefaultMode();
		} else {
			throw new DescriptorParsingException("Invalid bean instantiation mode '" + instModeCode + "'");
		}
	}

	@XmlAttribute(name = "conflict")
	protected final String getConflictModeCode() {
		return conflictResolutionMode.getCanonicalName();
	}

	protected final void setConflictModeCode(String conflictModeCode) {
		final BeanConflictMode mode = parseEnumByName(conflictModeCode, BeanConflictMode.class);
		if (null != mode) {
			this.conflictResolutionMode = mode;
		} else if (isBlank(conflictModeCode) && !isStrictParsingEnabled()) {
			this.conflictResolutionMode = BeanConflictMode.getDefaultMode();
		} else {
			throw new DescriptorParsingException("Invalid bean conflict mode '" + conflictModeCode + "'");
		}
	}

	@Override
	public int hashCode() {
		return (null != name) ? name.hashCode() : 0;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		} else if ((null == obj) || !getClass().isAssignableFrom(obj.getClass())) {
			// This works as an "instanceof" condition without hard-coding the constraint class, allowing
			// usage from subclasses
			return false;
		}
		assert obj instanceof AbstractBean;
		assert null != name;
		final AbstractBean other = (AbstractBean) obj;
		if (!name.equals(other.getName())) {
			return false;
		} else if (instantiationMode != other.getInstantiationMode()) {
			return false;
		} else if (conflictResolutionMode != other.getConflictResolutionMode()) {
			return false;
		}
		return true;
	}

}
