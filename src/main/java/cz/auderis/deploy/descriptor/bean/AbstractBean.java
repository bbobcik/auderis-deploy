package cz.auderis.deploy.descriptor.bean;

import cz.auderis.deploy.descriptor.DeploymentEntry;
import cz.auderis.deploy.descriptor.DescriptorParsingException;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlTransient;

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

	public abstract BeanType getBeanType();

	public abstract String getBeanClassName();

	public AbstractBean() {
		this.instantiationMode = BeanInstantiationMode.getDefaultMode();
		this.conflictResolutionMode = BeanConflictMode.getDefaultMode();
	}

	protected AbstractBean(String name, BeanInstantiationMode instMode, BeanConflictMode conflictMode) {
		assert null != name;
		assert null != instMode;
		assert null != conflictMode;
		this.name = name;
		this.instantiationMode = instMode;
		this.conflictResolutionMode = conflictMode;
	}

	public String getName() {
		return name;
	}

	public BeanInstantiationMode getInstantiationMode() {
		return instantiationMode;
	}

	public BeanConflictMode getConflictResolutionMode() {
		return conflictResolutionMode;
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
