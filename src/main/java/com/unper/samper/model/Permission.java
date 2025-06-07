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

import org.hibernate.annotations.Filter;
import org.hibernate.annotations.FilterDef;
import org.hibernate.annotations.ParamDef;
import org.hibernate.annotations.SQLDelete;

import com.unper.samper.model.common.Audit;
import com.unper.samper.model.constant.EPermissionReason;
import com.unper.samper.model.constant.EPermissionStatus;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Getter
@Setter
@Entity
@Table(name = "permission", schema = "schedule")
@SQLDelete(sql = "UPDATE public.permission SET is_deleted = true WHERE id=?")
@FilterDef(name = "deletedProductFilter", parameters = @ParamDef(name = "isDeleted", type = "boolean"))
@Filter(name = "deletedProductFilter", condition = "isDeleted = :isDeleted")
public class Permission extends Audit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = Student.class)
    @JoinColumn(name = "student_id")
    private Student student;

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = Schedule.class)
    @JoinColumn(name = "schedule_id")
    private Schedule schedule;

    @Enumerated(EnumType.STRING)
    private EPermissionReason reason;

    @Enumerated(EnumType.STRING)
    private EPermissionStatus status;

    @Builder.Default
    private Boolean isDeleted = Boolean.FALSE;
}
