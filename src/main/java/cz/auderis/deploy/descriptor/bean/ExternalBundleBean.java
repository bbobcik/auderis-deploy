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

import cz.auderis.deploy.descriptor.DescriptorParsingException;
import cz.auderis.deploy.descriptor.initializer.ExternalResourceElement;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.DeploymentVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isBlank;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.isStrictParsingEnabled;
import static cz.auderis.deploy.descriptor.DescriptorParserSupport.parseEnumByName;

@XmlRootElement(name = "propertyBundle")
@XmlType()
public class ExternalBundleBean extends AbstractBean {
	private static final long serialVersionUID = 20150728L;

	@XmlElementRef(type = ExternalResourceElement.class, required = false)
	protected List<ExternalResourceElement> resources;

	@XmlTransient
	protected ExternalBundleSourceMode sourceMode;

	public ExternalBundleBean() {
		super(BeanType.EXTERNAL_BUNDLE);
		this.resources = new ArrayList<ExternalResourceElement>(1);
		this.sourceMode = ExternalBundleSourceMode.getDefaultMode();
	}

	@Override
	public String getBeanClassName() {
		return ResourceBundle.class.getName();
	}

	public ExternalBundleSourceMode getSourceMode() {
		return sourceMode;
	}

	public List<ExternalResourceElement> getResources() {
		return resources;
	}

	@XmlAttribute(name = "useResource")
	protected final String getSourceModeCode() {
		return sourceMode.getCanonicalName();
	}

	protected final void setSourceModeCode(String sourceModeCode) {
		final ExternalBundleSourceMode mode = parseEnumByName(sourceModeCode, ExternalBundleSourceMode.class);
		if (null != mode) {
			this.sourceMode = mode;
		} else if (isBlank(sourceModeCode) && !isStrictParsingEnabled()) {
			this.sourceMode = ExternalBundleSourceMode.getDefaultMode();
		} else {
			throw new DescriptorParsingException("Invalid bundle source mode '" + sourceModeCode + "'");
		}
	}

	@Override
	public void updateDefinition(AbstractBean updatingAbstractBean) {
		super.updateDefinition(updatingAbstractBean);
		//
		assert updatingAbstractBean instanceof ExternalBundleBean;
		final ExternalBundleBean updatingBean = (ExternalBundleBean) updatingAbstractBean;
		// Replace source mode
		this.sourceMode = updatingBean.getSourceMode();

	}

	@Override
	public void accept(DeploymentVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			if (visitor instanceof DeploymentStructureVisitor) {
				final DeploymentStructureVisitor structVisitor = (DeploymentStructureVisitor) visitor;
				final boolean parentFirst = (VisitorContext.VisitOrder.PARENT_FIRST == context.getVisitOrder());
				if (parentFirst) {
					visitor.visitBundleBean(this);
				}
				for (final ExternalResourceElement resource : resources) {
					resource.accept(structVisitor, context);
				}
				if (!parentFirst) {
					visitor.visitBundleBean(this);
				}
			} else {
				visitor.visitBundleBean(this);
			}
		} finally {
			context.popContextPart();
		}
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		} else if (!super.equals(obj)) {
			return false;
		}
		assert obj instanceof ExternalBundleBean;
		final ExternalBundleBean other = (ExternalBundleBean) obj;
		if (sourceMode != other.getSourceMode()) {
			return false;
		}
		return true;
	}

}
