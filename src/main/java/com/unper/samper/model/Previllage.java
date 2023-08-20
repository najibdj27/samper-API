package com.unper.samper.model;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.unper.samper.model.common.Audit;
import com.unper.samper.model.constant.EPrevillage;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
@Entity
@Table(name = "previllage", schema = "public")
public class Previllage extends Audit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = BasePrevillage.class)
    @JoinColumn(name = "base_previllage_id")
    private BasePrevillage BasePrevillage;

    private String url;

    @Enumerated(EnumType.STRING)
    private EPrevillage previllage;
}
