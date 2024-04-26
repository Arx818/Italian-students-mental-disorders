import pandas as pd


class Report:
    """Report contains the dataframe..."""

    def __init__(self, data, **kargs):
        """If data is a filepath(string), then
        load a csv file into a pandas dataframe,
        else set as current dataframe"""
        if isinstance(data, str):
            self.df = pd.read_csv(data, **kargs)
        elif isinstance(data, pd.DataFrame):
            self.df = data

    def phase(self, phase):
        """Select a Lockdown phase and filter the dataframe"""
        if phase == "Before":
            mask = self.df.redcap_event_name == "Before Lockdown"
            self.df = self.df[mask]
        elif phase == "During":
            mask = self.df.redcap_event_name == "During Lockdown"
            self.df = self.df[mask]
        elif phase == "After":
            mask = self.df.redcap_event_name == "After Lockdown"
            self.df = self.df[mask]

    def females(self):
        """Filter dataframe for females"""
        mask = (self.df.sex == 'Females')
        self.df = self.df[mask]

    def males(self):
        """Filter dataframe for males"""
        mask = (self.df.sex == 'Males')
        self.df = self.df[mask]

    def disorder(self, flag):
        """Filter dataframe for disorder history"""
        if flag:
            mask = self.df.distvit___7 == 'Disorder'
            self.df = self.df[mask]
        else:
            mask = self.df.distvit___7 == 'No Disorder'
            self.df = self.df[mask]

    # OCI #
    def oci_score(self):
        """Return a tuple with OCI-R mediane, mean and IQR"""
        return (self.df.ocitot.median(),
                self.df.ocitot.mean(),
                (self.df.ocitot.quantile(.25),
                 self.df.ocitot.quantile(.75)))

    def oci_sum(self):
        """Return a sum of OCI-R scores"""
        return self.df.ocitot

    def oci_diff(self):
        """Return the differences between before and during"""
        return self.df.docitot

    def oci_difference(self):
        """Return the differences between covid phases of OCI-R
        scores"""
        return (self.df.docitot.median(),
                self.df.docitot.mean(),
                (self.df.docitot.quantile(.25),
                self.df.docitot.quantile(.75)))

    # BAI #
    def bai_score(self):
        """Return a tuple with BAI mediane, mean and IQR"""
        return (self.df.baitot.median(),
                self.df.baitot.mean(),
                (self.df.baitot.quantile(.25),
                 self.df.baitot.quantile(.75)))

    def bai_sum(self):
        """Return the sum of the BAI scores"""
        return self.df.baitot

    def bai_diff(self):
        """Return the differences between before and during"""
        return self.df.dBAI

    def bai_difference(self):
        """Return the differences between covid phases of BAI
        scores"""
        return (self.df.dBAI.median(),
                self.df.dBAI.mean(),
                (self.df.dBAI.quantile(.25),
                self.df.dBAI.quantile(.75)))

    # BDI #
    def bdi_score(self):
        """Return a tuple with BDI mediane, mean and IQR"""
        return (self.df.bditot.median(),
                self.df.bditot.mean(),
                (self.df.bditot.quantile(.25),
                 self.df.bditot.quantile(.75)))

    def bdi_sum(self):
        """Return the sum of BDI scores"""
        return self.df.bditot

    def bdi_diff(self):
        """Return the differences between before and during"""
        return self.df.dBDI

    def bdi_difference(self):
        """Return the differences between covid phases of BDI
        scores"""
        return (self.df.dBDI.median(),
                self.df.dBDI.mean(),
                (self.df.dBDI.quantile(.25),
                self.df.dBDI.quantile(.75)))

    # EHQ #
    def ehq_score(self):
        """Return a tuple with EHQ mediane, mean and IQR"""
        return (self.df.EHQtotal.median(),
                self.df.EHQtotal.mean(),
                (self.df.EHQtotal.quantile(.25),
                 self.df.EHQtotal.quantile(.75)))

    def ehq_sum(self):
        """Return the sums of EHQ scores"""
        return self.df.EHQtotal

    def ehq_diff(self):
        """Return the differences between before and during"""
        return self.df.dEHQTOT

    def ehq_difference(self):
        """Return the differences between covid phases of EHQ
        scores"""
        return (self.df.dEHQTOT.median(),
                self.df.dEHQTOT.mean(),
                (self.df.dEHQTOT.quantile(.25),
                self.df.dEHQTOT.quantile(.75)))

    # ED Risk #
    def ed_score(self):
        """Return a tuple with ED mediane, mean and IQR"""
        return (self.df.ED_risk.median(),
                self.df.ED_risk.mean(),
                (self.df.ED_risk.quantile(.25),
                 self.df.ED_risk.quantile(.75)))

    def ed_sum(self):
        """Return the sums of ED scores"""
        return self.df.ED_risk

    def ed_diff(self):
        """Return the differences between before and during"""
        return self.df.dEDI

    def ed_difference(self):
        """Return the differences between covid phases of ED
        scores"""
        return (self.df.dEDI.median(),
                self.df.dEDI.mean(),
                (self.df.dEDI.quantile(.25),
                self.df.dEDI.quantile(.75)))

    def show(self):
        """Return the current dataframe"""
        return self.df
