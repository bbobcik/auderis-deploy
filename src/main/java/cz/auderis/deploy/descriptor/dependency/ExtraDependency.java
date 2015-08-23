package cz.auderis.deploy.descriptor.dependency;


import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitor;
import cz.auderis.deploy.descriptor.visitor.VisitableStructuralNode;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;

@XmlRootElement(name = "dependsOn")
@XmlType(name = "extraDependencyType")
public class ExtraDependency implements VisitableStructuralNode, Serializable{
	private static final long serialVersionUID = 20150821L;

	@XmlAttribute(name = "bean", required = true)
	protected String beanName;

	public String getBeanName() {
		return beanName;
	}

	@Override
	public void accept(DeploymentStructureVisitor visitor, VisitorContext context) {
		context.pushContextPart(this);
		try {
			visitor.visitExtraDependency(this);
		} finally {
			context.popContextPart();
		}
	}
}
