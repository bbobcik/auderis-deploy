package cz.auderis.deploy.descriptor;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@XmlRootElement(name = "deployment")
@XmlType
public class Deployment implements Serializable {
	private static final long serialVersionUID = 20150728L;

	@XmlElementRef(type = DeploymentEntry.class)
	private List<DeploymentEntry> entries;

	public Deployment() {
		this.entries = new ArrayList<DeploymentEntry>(8);
	}

	public List<DeploymentEntry> getEntries() {
		if (null == entries) {
			return Collections.emptyList();
		}
		return entries;
	}

}
